"""
Wrapper de SB-VAE para el suite -- YA FUNCIONAL, en PyTorch puro.

A diferencia del código original de los autores (Theano, descontinuado),
esta versión usa pytorch_sbvae/model.py (reimplementación propia, ver ese
archivo para detalles y referencia al paper). Corre en el MISMO entorno que
DEC, DSC-Net, VaDE y DeepDPM (dcs-pytorch) -- ya no hace falta el entorno
legacy de Theano.

Tras entrenar, se usa el embedding de proporciones stick-breaking (pi) como
representación para clustering: se le aplica K-means sobre pi para obtener
las etiquetas finales (el propio modelo ya "apaga" dimensiones sobrantes
via la KL, así que K puede ser generoso -- el número de clusters real se
decide en el paso de K-means/posprocesado, igual que en DEC/VaDE).

Uso:
    conda activate dcs-pytorch
    python run_sbvae.py --data ../../data/mis_features.npy \
                         --labels ../../data/labels.npy \
                         --n_clusters 10 --K 50 --epochs 100 \
                         --out ../../results/sbvae
"""
import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "common"))
sys.path.insert(0, str(Path(__file__).resolve().parent))

from data_loader import load_tabular
from metrics import evaluate_all, print_report
from pytorch_sbvae.model import StickBreakingVAE, train_sbvae

import numpy as np
import torch
from sklearn.cluster import KMeans


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--data", required=True)
    ap.add_argument("--labels", default=None)
    ap.add_argument("--n_clusters", type=int, required=True,
                     help="clusters finales para K-means sobre el embedding")
    ap.add_argument("--K", type=int, default=50,
                     help="techo de dimensiones stick-breaking (K-1 sticks)")
    ap.add_argument("--hidden_dim", type=int, default=256)
    ap.add_argument("--epochs", type=int, default=100)
    ap.add_argument("--batch_size", type=int, default=64)
    ap.add_argument("--lr", type=float, default=1e-3)
    ap.add_argument("--out", default="../../results/sbvae")
    args = ap.parse_args()

    X, y = load_tabular(args.data, labels_path=args.labels)
    print(f"[SB-VAE] datos cargados: {X.shape}")

    torch.manual_seed(0)
    X_t = torch.from_numpy(X)

    model = StickBreakingVAE(
        input_dim=X.shape[1],
        K=args.K,
        hidden_dim=args.hidden_dim,
        prior_alpha=1.0,
        likelihood="gaussian",  # cambia a "bernoulli" si tus datos son binarios/imágenes en [0,1]
    )
    model = train_sbvae(
        model, X_t, epochs=args.epochs, batch_size=args.batch_size,
        lr=args.lr, device="cpu",
    )

    model.eval()
    with torch.no_grad():
        pi = model.encode_stick_proportions(X_t).numpy()

    km = KMeans(n_clusters=args.n_clusters, n_init=10, random_state=0)
    y_pred = km.fit_predict(pi)

    out_dir = Path(args.out)
    out_dir.mkdir(parents=True, exist_ok=True)
    np.save(out_dir / "preds.npy", y_pred)
    np.save(out_dir / "embedding.npy", pi)
    torch.save(model.state_dict(), out_dir / "model.pt")

    if y is not None:
        metrics = evaluate_all(y, y_pred)
        print_report("SB-VAE", metrics)
    else:
        print(f"[SB-VAE] listo, sin labels para evaluar. Predicciones en {out_dir}")


if __name__ == "__main__":
    main()
