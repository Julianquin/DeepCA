"""
Wrapper de DSC-Net (reimplementación PyTorch de XifengGuo) para el suite.

Requiere: entorno dcs-pytorch (ver envs/environment_pytorch.yml)
Requiere: models/dscnet_src/ (clonado de https://github.com/XifengGuo/DSC-Net)

*** ADVERTENCIA DE RENDIMIENTO (importante en CPU) ***
DSC-Net usa una capa "self-expressive" con una matriz de afinidad N x N
(N = número de muestras). Esto escala en O(N^2) en memoria y cómputo.
En CPU, mantén N pequeño (unos cientos, idealmente < 1000-2000 muestras)
o el entrenamiento será extremadamente lento / se quedará sin memoria.
Si tu dataset es grande, sub-muestrea antes de pasarlo a este modelo.

DSC-Net espera imágenes (usa capas convolucionales), no vectores tabulares
sueltos -- si tu dato es tabular, tendrías que adaptar el encoder/decoder
a capas densas (revisa dscnet_src/main.py).

Uso:
    conda activate dcs-pytorch
    python run_dscnet.py --data ../../data/mis_imagenes/ \
                          --n_clusters 5 \
                          --max_samples 800
"""
import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "common"))
sys.path.insert(0, str(Path(__file__).resolve().parent / "dscnet_src"))

from data_loader import load_images
from metrics import evaluate_all, print_report
import numpy as np


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--data", required=True, help="carpeta de imágenes")
    ap.add_argument("--n_clusters", type=int, required=True)
    ap.add_argument("--max_samples", type=int, default=800,
                     help="submuestreo obligatorio en CPU (ver advertencia)")
    ap.add_argument("--image_size", type=int, default=32)
    args = ap.parse_args()

    X, y = load_images(
        args.data,
        image_size=(args.image_size, args.image_size),
        grayscale=True,
        flatten=False,  # DSC-Net quiere N x C x H x W
    )

    if X.shape[0] > args.max_samples:
        print(f"[DSC-Net] submuestreando {X.shape[0]} -> {args.max_samples} "
              f"(evita O(N^2) en CPU)")
        idx = np.random.choice(X.shape[0], args.max_samples, replace=False)
        X = X[idx]
        if y is not None:
            y = y[idx]

    print(f"[DSC-Net] datos cargados: {X.shape}")

    # --- PUNTO DE INTEGRACIÓN ---
    # main.py de dscnet_src está pensado para correrse como script CLI
    # (python main.py --db coil20), leyendo datos desde .mat en su propio
    # formato. La integración más simple es:
    #   1. Guardar X, y en el formato .mat que main.py espera
    #      (revisa yaleb.py / post_clustering.py del repo para el esquema)
    #   2. Invocar dscnet_src/main.py --db custom apuntando a ese .mat
    # o alternativamente refactorizar main.py para aceptar un array en
    # memoria directamente (más limpio pero requiere editar el repo).
    raise NotImplementedError(
        "Conecta aquí con dscnet_src/main.py. Lo más simple es exportar X "
        "a un .mat compatible con el loader del repo (ver dscnet_src/yaleb.py)."
    )


if __name__ == "__main__":
    main()
