"""
Wrapper de DEC (pt-dec) para el suite.

Requiere: entorno dcs-pytorch (ver envs/environment_pytorch.yml)
Requiere: models/dec_src/ (clonado de https://github.com/vlukiyanov/pt-dec)

Uso:
    conda activate dcs-pytorch
    python run_dec.py --data ../../data/mis_features.npy \
                       --labels ../../data/labels.npy \
                       --n_clusters 10 \
                       --out ../../results/dec

TODO antes de correr con tus datos reales:
  1. Revisa examples/mnist/mnist.py dentro de dec_src/ como referencia de uso.
  2. Ajusta el autoencoder (n_input, capas) a la dimensionalidad de tus datos
     en dec_src/ptdec/ (o pásalo como parámetro si el repo lo permite).
  3. Con CPU y datasets grandes, reduce --epochs y --batch_size.
"""
import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "common"))
sys.path.insert(0, str(Path(__file__).resolve().parent / "dec_src"))

from data_loader import load_tabular
from metrics import evaluate_all, print_report
import numpy as np


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--data", required=True)
    ap.add_argument("--labels", default=None)
    ap.add_argument("--n_clusters", type=int, required=True)
    ap.add_argument("--epochs", type=int, default=50)
    ap.add_argument("--batch_size", type=int, default=64)
    ap.add_argument("--out", default="../../results/dec")
    args = ap.parse_args()

    X, y = load_tabular(args.data, labels_path=args.labels)
    print(f"[DEC] datos cargados: {X.shape}")

    # --- PUNTO DE INTEGRACIÓN ---
    # Aquí es donde se conecta con ptdec (pt-dec). La API exacta de pt-dec
    # requiere construir un StackedDenoisingAutoEncoder + DEC de
    # ptdec.model y ptdec.dec -- revisa examples/mnist/mnist.py en dec_src/
    # para el patrón exacto (dataset -> pretrain autoencoder -> DEC.fit).
    #
    # from ptdec.dec import DEC
    # from ptdec.model import train, predict
    # from ptdec.utils import cluster_accuracy
    # ... (adaptar dataset de pt-dec a partir de X)
    raise NotImplementedError(
        "Conecta aquí la API de ptdec (ver dec_src/examples/mnist/mnist.py) "
        "usando X como entrada. Esqueleto listo, falta el 'pegamento' final "
        "porque pt-dec espera un torch.utils.data.Dataset propio."
    )


if __name__ == "__main__":
    main()
