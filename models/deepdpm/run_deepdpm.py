"""
Wrapper de DeepDPM para el suite.

Requiere: entorno dcs-pytorch (ver envs/environment_pytorch.yml)
Requiere: models/deepdpm_src/ (clonado de https://github.com/BGU-CS-VIL/DeepDPM)

DeepDPM NO requiere que definas --n_clusters: infiere el número de
clusters automáticamente. Es el más directo de integrar porque el repo
oficial ya trae scripts de línea de comandos listos.

Uso (según README de deepdpm_src):
    conda activate dcs-pytorch
    cd ../deepdpm_src
    python DeepDPM.py --dir <ruta_a_tus_datos_preprocesados> \
                       --dataset custom

IMPORTANTE: DeepDPM espera tensores .pt (train_data.pt / test_data.pt) en
--dir, no arrays crudos. Este script se encarga de la conversión desde el
formato común del suite.
"""
import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "common"))
from data_loader import load_tabular

import numpy as np
import torch


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--data", required=True)
    ap.add_argument("--labels", default=None)
    ap.add_argument("--staging_dir", default="./_deepdpm_staging")
    args = ap.parse_args()

    X, y = load_tabular(args.data, labels_path=args.labels)
    print(f"[DeepDPM] datos cargados: {X.shape}")

    staging = Path(args.staging_dir)
    staging.mkdir(parents=True, exist_ok=True)

    X_t = torch.from_numpy(X).float()
    torch.save(X_t, staging / "train_data.pt")
    torch.save(X_t, staging / "test_data.pt")  # DeepDPM espera ambos
    if y is not None:
        torch.save(torch.from_numpy(np.asarray(y)).long(), staging / "train_labels.pt")
        torch.save(torch.from_numpy(np.asarray(y)).long(), staging / "test_labels.pt")

    print(f"[DeepDPM] tensores listos en {staging.resolve()}")
    print(
        "Ahora corre desde deepdpm_src/:\n"
        f"  python DeepDPM.py --dir {staging.resolve()} --dataset custom "
        f"{'--use_labels_for_eval' if y is not None else ''}"
    )


if __name__ == "__main__":
    main()
