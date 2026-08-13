"""
Wrapper de VaDE (reimplementación PyTorch Lightning de ysterin) para el suite.

Requiere: entorno dcs-pytorch (ver envs/environment_pytorch.yml)
Requiere: models/vade_src/ (clonado de https://github.com/ysterin/VaDE)

Esta reimplementación usa un `data_modules.py` propio (pl.LightningDataModule).
Para tus datos, necesitas envolver X en un DataModule compatible -- ver
vade_src/data_modules.py como referencia (ahí verás los datasets que ya trae,
p.ej. MNIST) y replicar la clase con tu np.ndarray en vez de torchvision.

Uso:
    conda activate dcs-pytorch
    python run_vade.py --data ../../data/mis_features.npy \
                        --labels ../../data/labels.npy \
                        --n_clusters 10
"""
import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "common"))
sys.path.insert(0, str(Path(__file__).resolve().parent / "vade_src"))

from data_loader import load_tabular
from metrics import evaluate_all, print_report


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--data", required=True)
    ap.add_argument("--labels", default=None)
    ap.add_argument("--n_clusters", type=int, required=True)
    ap.add_argument("--max_epochs", type=int, default=100)
    args = ap.parse_args()

    X, y = load_tabular(args.data, labels_path=args.labels)
    print(f"[VaDE] datos cargados: {X.shape}")

    # --- PUNTO DE INTEGRACIÓN ---
    # from pl_modules import VaDE  (clase definida en vade_src/pl_modules.py)
    # import pytorch_lightning as pl
    #
    # 1. Crear un DataModule propio que envuelva X (ver data_modules.py)
    # 2. model = VaDE(n_clusters=args.n_clusters, input_dim=X.shape[1], ...)
    # 3. trainer = pl.Trainer(max_epochs=args.max_epochs, accelerator="cpu")
    # 4. trainer.fit(model, datamodule=mi_datamodule)
    # 5. y_pred = model.predict_clusters(X)  (revisar nombre exacto del método
    #    en pl_modules.py -- puede llamarse distinto)
    raise NotImplementedError(
        "Conecta aquí VaDE de vade_src/pl_modules.py. Necesitas escribir un "
        "DataModule para tu np.ndarray (ver vade_src/data_modules.py como "
        "plantilla)."
    )


if __name__ == "__main__":
    main()
