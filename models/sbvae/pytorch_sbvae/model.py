"""
Stick-Breaking Variational Autoencoder (SB-VAE) en PyTorch puro.

Reimplementación propia basada en:
  Nalisnick, E. & Smyth, P. (2017). "Stick-Breaking Variational Autoencoders".
  ICLR 2017. https://arxiv.org/abs/1605.06197

El código original de los autores está en Theano (ver
github.com/enalisnick/stick-breaking_dgms) y ya no se mantiene. Esta versión
reproduce la idea central del paper -- reemplazar el prior Gaussiano del VAE
por un proceso stick-breaking con dimensionalidad latente estocástica -- pero
usando PyTorch moderno, para poder correr en el mismo entorno que los demás
modelos del suite (dcs-pytorch).

Diferencia clave respecto al VAE estándar:
  En vez de muestrear z ~ N(mu, sigma^2), se muestrean K-1 variables
  "stick-breaking" v_k ~ Kumaraswamy(a_k, b_k) (reparametrizable, análoga
  a Beta pero con inversa de CDF en forma cerrada) y se construye:
      pi_1 = v_1
      pi_k = v_k * prod_{i<k} (1 - v_i)      para k = 2..K-1
      pi_K = prod_{i=1}^{K-1} (1 - v_i)
  obteniendo un vector de proporciones (pi_1..pi_K) que suma 1 -- este es el
  "código" latente que usa el decoder. Con K grande y la KL empujando v_k
  hacia el prior, el modelo tiende a "apagar" dimensiones sobrantes, dando
  una dimensionalidad latente efectiva menor que K (de ahí "stochastic
  dimensionality" en el paper).

Uso como librería (ver run_sbvae.py para el flujo completo):

    from pytorch_sbvae.model import StickBreakingVAE
    model = StickBreakingVAE(input_dim=X.shape[1], K=50, hidden_dim=256)
    ...  # entrenar con train_sbvae() de este mismo módulo
    pi = model.encode_stick_proportions(X)   # usar como embedding para clustering
"""
from __future__ import annotations
import math
import torch
import torch.nn as nn
import torch.nn.functional as F


# --------------------------------------------------------------------------
# Aproximación de la KL(Kumaraswamy(a,b) || Beta(alpha_prior, 1))
# Fórmula (10) del paper, truncando la serie infinita a M términos.
# --------------------------------------------------------------------------
EULER_GAMMA = 0.5772156649015329


def kumaraswamy_beta_kl(a: torch.Tensor, b: torch.Tensor, prior_alpha: float,
                         n_terms: int = 10) -> torch.Tensor:
    """KL(Kumaraswamy(a,b) || Beta(prior_alpha, 1)), aproximada por serie.

    a, b: tensores positivos de forma (batch, K-1)
    Devuelve un tensor (batch, K-1) con la KL por dimensión stick-breaking.
    """
    a = a.clamp(min=1e-4)
    b = b.clamp(min=1e-4)

    digamma_b = torch.digamma(b)

    # término de la serie: sum_{m=1}^{M} 1/(m + a*b) * B(m/a, b)
    # B(x,y) = exp(lgamma(x)+lgamma(y)-lgamma(x+y))
    series = torch.zeros_like(a)
    for m in range(1, n_terms + 1):
        m_t = float(m)
        log_beta_term = (
            torch.lgamma(m_t / a) + torch.lgamma(b) - torch.lgamma(m_t / a + b)
        )
        series = series + torch.exp(log_beta_term) / (m_t + a * b)

    kl = (
        (a - prior_alpha) / a * (-EULER_GAMMA - digamma_b - 1.0 / b)
        + torch.log(a * b + 1e-10)
        - math.log(prior_alpha)  # log B(alpha, 1) = -log(alpha) para Beta(alpha,1)
        - (b - 1.0) / b
        + series
    )
    return kl


class StickBreakingVAE(nn.Module):
    """SB-VAE con encoder/decoder MLP (adecuado para datos tabulares o
    imágenes aplanadas). Para imágenes con estructura espacial, reemplaza
    encoder/decoder por capas convolucionales si lo necesitas.
    """

    def __init__(
        self,
        input_dim: int,
        K: int = 50,
        hidden_dim: int = 256,
        prior_alpha: float = 1.0,
        likelihood: str = "gaussian",  # "gaussian" o "bernoulli"
    ):
        super().__init__()
        self.input_dim = input_dim
        self.K = K
        self.prior_alpha = prior_alpha
        self.likelihood = likelihood

        # Encoder: produce parámetros (a, b) de Kumaraswamy para K-1 sticks
        self.encoder = nn.Sequential(
            nn.Linear(input_dim, hidden_dim),
            nn.ReLU(),
            nn.Linear(hidden_dim, hidden_dim),
            nn.ReLU(),
        )
        self.fc_a = nn.Linear(hidden_dim, K - 1)
        self.fc_b = nn.Linear(hidden_dim, K - 1)

        # Decoder: de las K proporciones stick-breaking a la reconstrucción
        self.decoder = nn.Sequential(
            nn.Linear(K, hidden_dim),
            nn.ReLU(),
            nn.Linear(hidden_dim, hidden_dim),
            nn.ReLU(),
            nn.Linear(hidden_dim, input_dim),
        )

    def encode_params(self, x: torch.Tensor):
        h = self.encoder(x)
        # softplus asegura a, b > 0
        a = F.softplus(self.fc_a(h)) + 1e-4
        b = F.softplus(self.fc_b(h)) + 1e-4
        return a, b

    @staticmethod
    def sample_kumaraswamy(a: torch.Tensor, b: torch.Tensor) -> torch.Tensor:
        """Muestreo reparametrizado v ~ Kumaraswamy(a,b) vía inversa de CDF:
        v = (1 - u^(1/b))^(1/a),  u ~ Uniform(0,1)
        """
        u = torch.rand_like(a).clamp(1e-6, 1 - 1e-6)
        v = (1.0 - u.pow(1.0 / b)).pow(1.0 / a)
        return v.clamp(1e-6, 1 - 1e-6)

    @staticmethod
    def stick_breaking(v: torch.Tensor) -> torch.Tensor:
        """Convierte v_1..v_{K-1} en proporciones pi_1..pi_K que suman 1."""
        batch = v.shape[0]
        K_minus_1 = v.shape[1]
        # productos acumulados de (1 - v_i) para i < k
        one_minus_v = 1.0 - v
        cumprod = torch.cumprod(one_minus_v, dim=1)
        cumprod_shifted = torch.cat(
            [torch.ones(batch, 1, device=v.device), cumprod[:, :-1]], dim=1
        )
        pi_first = v * cumprod_shifted          # pi_1 .. pi_{K-1}
        pi_last = cumprod[:, -1:]                # pi_K = resto del stick
        pi = torch.cat([pi_first, pi_last], dim=1)
        return pi

    def encode_stick_proportions(self, x: torch.Tensor) -> torch.Tensor:
        """Usa la MEDIA de Kumaraswamy (determinista) -- útil para extraer
        el embedding final que se pasa a clustering (no para entrenar).
        """
        a, b = self.encode_params(x)
        # media aproximada de Kumaraswamy: b * B(1+1/a, b)
        mean_v = b * torch.exp(
            torch.lgamma(1 + 1.0 / a) + torch.lgamma(b) - torch.lgamma(1 + 1.0 / a + b)
        )
        pi = self.stick_breaking(mean_v.clamp(1e-6, 1 - 1e-6))
        return pi

    def forward(self, x: torch.Tensor):
        a, b = self.encode_params(x)
        v = self.sample_kumaraswamy(a, b)
        pi = self.stick_breaking(v)
        x_recon = self.decoder(pi)
        return x_recon, a, b, pi

    def loss(self, x: torch.Tensor):
        x_recon, a, b, pi = self.forward(x)

        if self.likelihood == "bernoulli":
            recon_loss = F.binary_cross_entropy_with_logits(
                x_recon, x, reduction="none"
            ).sum(dim=1)
        else:
            recon_loss = F.mse_loss(x_recon, x, reduction="none").sum(dim=1)

        kl = kumaraswamy_beta_kl(a, b, self.prior_alpha).sum(dim=1)
        elbo_loss = (recon_loss + kl).mean()
        return elbo_loss, recon_loss.mean().item(), kl.mean().item()


def train_sbvae(
    model: StickBreakingVAE,
    X: torch.Tensor,
    epochs: int = 100,
    batch_size: int = 64,
    lr: float = 1e-3,
    device: str = "cpu",
    verbose: bool = True,
):
    """Loop de entrenamiento simple. X: tensor (N, input_dim) ya normalizado."""
    model = model.to(device)
    X = X.to(device)
    optimizer = torch.optim.Adam(model.parameters(), lr=lr)
    n = X.shape[0]

    for epoch in range(epochs):
        perm = torch.randperm(n)
        total_loss = 0.0
        for i in range(0, n, batch_size):
            idx = perm[i : i + batch_size]
            batch = X[idx]

            optimizer.zero_grad()
            loss, recon, kl = model.loss(batch)
            loss.backward()
            optimizer.step()
            total_loss += loss.item() * batch.shape[0]

        if verbose and (epoch % max(1, epochs // 10) == 0 or epoch == epochs - 1):
            print(f"[SB-VAE] epoch {epoch+1}/{epochs}  loss={total_loss/n:.4f}")

    return model
