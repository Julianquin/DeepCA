"""
Orquestador: NO ejecuta los modelos en un solo proceso Python (LSPCM sigue
necesitando R por debajo -- ver README.md), sino que:

  1. Te dice exactamente qué comando correr para cada modelo. 5 de los 6
     corren en el mismo entorno dcs-pytorch; LSPCM se dispara desde ahí
     mismo pero internamente invoca R vía subprocess.
  2. Una vez que corriste cada uno y guardaste sus predicciones en
     results/<modelo>/preds.npy (o preds.csv para LSPCM), este script las
     junta, calcula las métricas (ACC/NMI/ARI) de forma comparable y arma
     una tabla final.

Uso típico:

  # Paso 1: ver el plan de comandos
  python run_all.py plan --data ../data/mis_features.npy \
                          --labels ../data/labels.npy --n_clusters 10

  # Paso 2: correr cada comando (todo desde el mismo entorno dcs-pytorch)

  # Paso 3: comparar resultados una vez tengas los preds.npy/.csv de cada uno
  python run_all.py compare --labels ../data/labels.npy
"""
import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from metrics import evaluate_all, print_report
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]

PLAN = """
==================== PLAN DE EJECUCIÓN ====================

Todos los modelos son NO supervisados: las labels solo se usan al final
para evaluar, nunca para entrenar.

--- Entorno único dcs-pytorch (DEC, DSC-Net, VaDE, DeepDPM, SB-VAE) ---
conda env create -f envs/environment_pytorch.yml
conda activate dcs-pytorch

  1) DEC:
     cd models/dec && python run_dec.py --data {data} --labels {labels} \\
         --n_clusters {k} --out ../../results/dec

  2) DSC-Net (solo imágenes, submuestreo obligatorio en CPU):
     cd models/dscnet && python run_dscnet.py --data {data} \\
         --n_clusters {k} --max_samples 800

  3) VaDE:
     cd models/vade && python run_vade.py --data {data} --labels {labels} \\
         --n_clusters {k}

  4) DeepDPM (no necesita --n_clusters, lo infiere):
     cd models/deepdpm && python run_deepdpm.py --data {data} --labels {labels}
     # luego seguir la instrucción impresa por ese script

  5) SB-VAE (reimplementación propia en PyTorch, ya funcional):
     cd models/sbvae && python run_sbvae.py --data {data} --labels {labels} \\
         --n_clusters {k} --K 50 --epochs 100

--- LSPCM (SOLO si tus datos son de red/grafo) ---
No requiere activar otro entorno de Python: se dispara desde el mismo
dcs-pytorch vía subprocess (necesita R + paquete `lspm` instalados en el
sistema, ver envs/environment_r.txt). Se mantiene en R -- y no se reescribe
en PyTorch -- porque es un muestreador MCMC Bayesiano hecho a mano, no una
red neuronal; reescribirlo no aporta velocidad y arriesga introducir bugs
sutiles respecto al código validado de los autores.

  6) LSPCM:
     cd models/lspcm_bridge && python run_lspcm_bridge.py \\
         --adjacency {adjacency} --labels {labels} \\
         --n_dimen 5 --G {k} --iter 50000 --burnin 5000 --thin 100

=============================================================
NOTA: cada run_*.py (excepto SB-VAE y LSPCM, ya funcionales) tiene un bloque
"PUNTO DE INTEGRACIÓN" marcado con NotImplementedError -- ahí falta conectar
la API específica de ese repo con tus datos. Son ~10-30 líneas de
"pegamento" por modelo; te puedo ayudar a completarlas en cuanto me digas
con qué modelo seguir.
"""


def cmd_plan(args):
    print(PLAN.format(
        data=args.data,
        labels=args.labels or "(sin labels)",
        k=args.n_clusters,
        adjacency=args.adjacency or "<ruta_matriz_adyacencia>",
    ))


def cmd_compare(args):
    results_dir = ROOT / "results"
    y_true = None
    if args.labels:
        yp = Path(args.labels)
        y_true = np.load(yp) if yp.suffix == ".npy" else np.loadtxt(yp, delimiter=",")

    found_any = False
    for model_dir in sorted(results_dir.iterdir()):
        pred_file_npy = model_dir / "preds.npy"
        pred_file_csv = model_dir / "preds.csv"  # LSPCM (R) guarda en .csv
        if pred_file_npy.exists():
            y_pred = np.load(pred_file_npy)
        elif pred_file_csv.exists():
            y_pred = pd.read_csv(pred_file_csv).values.ravel()
        else:
            continue
        found_any = True
        if y_true is not None:
            metrics = evaluate_all(y_true, y_pred)
            print_report(model_dir.name, metrics)
        else:
            print(f"{model_dir.name:12s} | (sin labels, no se puede evaluar ACC/NMI/ARI)")

    if not found_any:
        print("No se encontró ningún results/<modelo>/preds.npy todavía.")
        print("Corre primero los modelos (ver: python run_all.py plan ...)")


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest="cmd", required=True)

    p_plan = sub.add_parser("plan")
    p_plan.add_argument("--data", required=True)
    p_plan.add_argument("--labels", default=None)
    p_plan.add_argument("--n_clusters", type=int, default=10)
    p_plan.add_argument("--adjacency", default=None)
    p_plan.set_defaults(func=cmd_plan)

    p_cmp = sub.add_parser("compare")
    p_cmp.add_argument("--labels", default=None)
    p_cmp.set_defaults(func=cmd_compare)

    args = ap.parse_args()
    args.func(args)
