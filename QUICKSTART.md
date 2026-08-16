# Cómo empezar a probar los métodos

Los datos ya están en `data/` (ver [data/README.md](data/README.md)). Lo que
falta es el entorno. Este documento va en orden de "lo que corre hoy con menos
trabajo" a "lo que necesita más".

## Estado real de cada modelo

| Modelo | Datos | Código | Bloqueo |
|---|---|---|---|
| **DSC-Net** | ✅ `.mat` en `models/dscnet_src/datasets/` | ✅ `main.py` oficial | Solo falta torch |
| **SB-VAE** | ✅ `data/mnist/` | ✅ `run_sbvae.py` funcional | Solo falta torch |
| **DeepDPM** | ✅ `data/deepdpm_embeddings/` | ⚠️ funcional pero con **PyTorch Lightning 1.x** | Necesita stack antiguo o parche |
| **VaDE** | ✅ `data/mnist/` | ❌ `NotImplementedError` + PL 1.x + `gpus=1` hardcodeado | Pegamento + stack antiguo |
| **DEC** | ✅ `data/mnist/` | ❌ `NotImplementedError` | Pegamento + `ptsdae` (no está en PyPI) |
| **LSPCM** | ✅ `data/lspcm_football/`, `data/lspcm_irish/` | ✅ R completo | R no está instalado |

---

## Paso 0: entorno

`.venv/` ya no existe (lo borraste después del último commit), así que hay que
recrearlo. **Hay un conflicto de versiones que conviene decidir antes:**

- **DSC-Net y SB-VAE** corren con PyTorch moderno (2.x) y Python 3.12.
- **DeepDPM y VaDE** usan la API de **PyTorch Lightning 1.x**, que desapareció
  en PL 2.0. Concretamente:
  - `from pytorch_lightning.loggers.base import DummyLogger` → el módulo
    `loggers.base` ya no existe.
  - `pl.Trainer(gpus=..., checkpoint_callback=..., progress_bar_refresh_rate=...)`
    → los tres argumentos se eliminaron en PL 2.0.

  `models/deepdpm_src/requirements.txt` pide `pytorch_lightning==1.2.10` y
  `torch==1.11.0`, que no tienen wheels para Python 3.12.

**Ojo:** `envs/environment_pytorch.yml` no resuelve esto — pide `torch=2.2` y
`pytorch-lightning` sin pinear, lo que instala PL 2.x y rompe DeepDPM y VaDE.

### Opción A — empezar ya con lo que funciona (recomendado)

Python 3.12 del sistema, sin conda:

```bash
cd ~/proyectos/DeepCA
python3 -m venv .venv
.venv/bin/pip install numpy scipy pandas scikit-learn pillow matplotlib tqdm munkres
.venv/bin/pip install torch torchvision --index-url https://download.pytorch.org/whl/cpu
```

El `--index-url` de CPU es importante: sin él, pip baja las wheels con CUDA
(~2.5 GB en vez de ~200 MB).

Con esto corren **DSC-Net** y **SB-VAE** (pasos 1 y 2).

### Opción B — además, entorno legacy para DeepDPM/VaDE

Requiere Miniconda (no hay conda instalado ahora mismo):

```bash
curl -LO https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
bash Miniconda3-latest-Linux-x86_64.sh -b -p ~/miniconda3
~/miniconda3/bin/conda create -y -n dcs-legacy python=3.10
~/miniconda3/bin/conda run -n dcs-legacy pip install \
    torch==1.11.0 torchvision==0.12.0 --index-url https://download.pytorch.org/whl/cpu
~/miniconda3/bin/conda run -n dcs-legacy pip install \
    pytorch_lightning==1.6.5 numpy scipy pandas scikit-learn matplotlib tqdm joblib umap-learn
```

(PL 1.6.5 en vez del 1.2.10 que pide el `requirements.txt`: sigue teniendo la
API vieja que DeepDPM usa, pero es bastante menos frágil.)

### Opción C — parchear DeepDPM para PL 2.x

Son 3-4 ediciones pequeñas (el import de `DummyLogger`, los argumentos del
`Trainer`, `pl.utilities.seed`). Deja todo en un solo entorno moderno, a costa
de tocar el código de los autores. Dime si prefieres esta y lo hago.

---

## Paso 1: DSC-Net en COIL20 — el más rápido (cero pegamento)

Es el único que corre **sin escribir nada**: los `.mat` ya están en
`models/dscnet_src/datasets/` y los pesos preentrenados del autor original en
`models/dscnet_src/pretrained_weights_original/`.

```bash
cd models/dscnet_src
../../.venv/bin/python main.py --db coil20
```

Referencia del repo: ACC 0.9100 / NMI 0.9587 en COIL20 (el paper reporta
0.9486 con la implementación original en TensorFlow).

Otros datasets ya listos: `--db coil100`, `--db orl --show-freq 100`, y
`python yaleb.py` para Extended Yale B.

Dos avisos:
- Para COIL20, descomenta la línea 64 de `post_clustering.py` para acercarte al
  número publicado (lo dice el propio README del repo).
- Si torch ≥ 2.6 se queja al cargar los `.pkl`, es por el cambio de
  `weights_only=True` por defecto en `torch.load`: añade `weights_only=False`
  en la línea 262 de `main.py`.

**No es un método no paramétrico** (necesita K y hace spectral clustering al
final), pero sirve para validar que el entorno y los datos están bien.

## Paso 2: SB-VAE en MNIST

```bash
cd models/sbvae
../../.venv/bin/python run_sbvae.py \
    --data ../../data/mnist/X.npy --labels ../../data/mnist/y.npy \
    --n_clusters 10 --K 50 --epochs 30 --out ../../results/sbvae
```

Guarda `preds.npy`, `embedding.npy` y `model.pt`, e imprime ACC/NMI/ARI.

Recuerda lo del review: el `--n_clusters` aquí es un KMeans encima del
embedding, así que **esto no es clustering no paramétrico**. Empieza con
`--epochs 30` para ver que arranca; 70000×784 en CPU no es instantáneo.

## Paso 3: DeepDPM — el que de verdad te interesa

Este es el único que infiere K. Con el entorno legacy (Opción B) y los
embeddings oficiales ya descargados:

```bash
cd models/deepdpm_src
~/miniconda3/bin/conda run -n dcs-legacy python DeepDPM.py \
    --dataset MNIST --dir ../../data/deepdpm_embeddings/MNIST --offline
```

No lleva `--n_clusters`: el K estimado sale en el log. En el paper, MNIST/UMAP
converge a K=10.

Otros ya listos en `data/deepdpm_embeddings/`:

```bash
# Fashion-MNIST y USPS (mismas tablas del paper)
python DeepDPM.py --dataset FASHION --dir ../../data/deepdpm_embeddings/FASHION --offline
python DeepDPM.py --dataset USPS    --dir ../../data/deepdpm_embeddings/USPS    --offline

# La prueba interesante: versiones desbalanceadas, donde los métodos
# paramétricos se caen y DeepDPM aguanta (Fig. 1 del paper)
python DeepDPM.py --dataset MNIST --dir ../../data/deepdpm_embeddings/MNIST_IMBALANCED --offline

# STL-10 con embeddings MoCo
python DeepDPM.py --dataset stl10 --init_k 3 \
    --dir ../../data/deepdpm_embeddings/STL10 \
    --NIW_prior_nu 514 --prior_sigma_scale 0.05 --offline

# ImageNet-50, sin necesidad de bajar ImageNet
python DeepDPM.py --dataset imagenet_50 --init_k 10 \
    --dir ../../data/deepdpm_embeddings/IMAGENET_50 --offline
```

Notas:
- `--offline` evita que intente conectarse a Neptune (el logger que usan).
- `STL10/test_codes.pt` se llama así en el repo original; si el script pide
  `test_data.pt`, cópialo con ese nombre.
- Para tus propios datos: `models/deepdpm/run_deepdpm.py --data ... --labels ...`
  convierte un `.npy` a los tensores `train_data.pt`/`test_data.pt` que espera
  `--dir`, y te imprime el comando exacto.

## Paso 4: LSPCM (solo si trabajas con redes)

```bash
sudo apt install -y r-base
R -e 'install.packages(c("abind","mvtnorm","sna","truncdist","vioplot","Rfast","MCMCpack","mclust","mcclust","rootSolve","seriation","remotes"))'
cd models/lspcm_src && R -e 'remotes::install_local(dependencies = TRUE)'

cd ../lspcm
Rscript run_lspcm.R --adjacency ../../data/lspcm_football/adjacency.csv \
    --labels ../../data/lspcm_football/labels.csv \
    --n_dimen 5 --G 20 --iter 5000 --burnin 500 --thin 20
```

Empieza por `lspcm_football` (55 nodos): es la red pequeña del paper y con
`--iter 5000` termina rápido. `lspcm_irish` (348 nodos) con los parámetros del
paper (2e6 iteraciones) tarda horas — es MCMC, no épocas.

## Paso 5: DEC y VaDE — requieren escribir el pegamento

Ambos tienen `NotImplementedError` a propósito. Para DEC hace falta además
instalar `ptsdae`, que **no está en PyPI** y cuyo `requirements.txt` lo pide con
el protocolo `git://` que GitHub desactivó en 2021:

```bash
.venv/bin/pip install git+https://github.com/vlukiyanov/pt-sdae.git
```

VaDE además tiene `gpus=1` hardcodeado en `train_vade.py:53` y
`pl_modules.py:79,143`, así que en CPU hay que tocarlo igualmente.

Ninguno de los dos es no paramétrico, así que si el objetivo es comparar contra
DeepDPM, tienen sentido como baselines — pero son los últimos de la fila.

---

## Orden sugerido

1. Opción A del entorno → **Paso 1 (DSC-Net/COIL20)**. Valida entorno + datos en
   minutos, sin escribir código.
2. **Paso 2 (SB-VAE/MNIST)** para validar el pipeline común
   (`data_loader` → modelo → `metrics` → `results/`).
3. Decide entre Opción B (conda legacy) u Opción C (parchear DeepDPM) y ve al
   **Paso 3**, que es el método que realmente responde a tu pregunta.
