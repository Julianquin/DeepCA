"""
Puente para correr LSPCM desde el mismo pipeline Python (entorno dcs-pytorch),
sin salir de tu script/notebook -- aunque el cómputo real siga ejecutándose
en R por debajo.

Por qué no está reescrito en PyTorch: LSPCM es un muestreador MCMC Bayesiano
(Metropolis-within-Gibbs) hecho a mano por los autores, no una red neuronal.
No hay gradientes ni GPU que aprovechar -- "portarlo a PyTorch" sería solo
traducir ~300 líneas de actualizaciones estadísticas artesanales (Gibbs,
Metropolis-Hastings adaptativo, rotación Procrustes, cambio dinámico de
dimensión) a otro lenguaje, con riesgo real de introducir errores sutiles
que no podría validar sin comparar exhaustivamente contra la implementación
original de los autores. Por eso este wrapper llama al R real y validado.

Requisitos:
  - R instalado en el sistema (Rscript en el PATH)
  - Paquete `lspm` instalado (ver ../lspcm/../../envs/environment_r.txt)

Uso desde Python (mismo entorno que los demás modelos):

    from run_lspcm_bridge import run_lspcm

    y_pred = run_lspcm(
        adjacency_csv="../../data/mi_red_adj.csv",
        n_dimen=5, G=20, iter=5000, burnin=500, thin=20,
        labels_csv="../../data/labels.csv",  # opcional
    )
"""
from __future__ import annotations
import subprocess
import shutil
from pathlib import Path
import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
R_SCRIPT = HERE.parent / "lspcm" / "run_lspcm.R"


def r_available() -> bool:
    return shutil.which("Rscript") is not None


def run_lspcm(
    adjacency_csv: str,
    n_dimen: int = 5,
    G: int = 20,
    iter: int = 50000,
    burnin: int = 5000,
    thin: int = 100,
    labels_csv: str | None = None,
    out_dir: str = "../../results/lspcm",
) -> np.ndarray:
    """Corre LSPCM invocando Rscript como subproceso y devuelve las
    predicciones como np.ndarray, para que encajen con el resto del
    pipeline Python (misma interfaz que los otros modelos: metrics.py,
    run_all.py, etc.)
    """
    if not r_available():
        raise RuntimeError(
            "Rscript no está disponible en el PATH. Instala R o corre "
            "models/lspcm/run_lspcm.R manualmente (ver envs/environment_r.txt)."
        )

    cmd = [
        "Rscript", str(R_SCRIPT),
        "--adjacency", adjacency_csv,
        "--n_dimen", str(n_dimen),
        "--G", str(G),
        "--iter", str(iter),
        "--burnin", str(burnin),
        "--thin", str(thin),
        "--out", out_dir,
    ]
    if labels_csv:
        cmd += ["--labels", labels_csv]

    print("[LSPCM bridge] ejecutando:", " ".join(cmd))
    result = subprocess.run(cmd, cwd=str(R_SCRIPT.parent), capture_output=True, text=True)
    print(result.stdout)
    if result.returncode != 0:
        print(result.stderr)
        raise RuntimeError("LSPCM (R) terminó con error, ver salida arriba.")

    preds_path = Path(out_dir) / "preds.csv"
    if not preds_path.is_absolute():
        preds_path = (R_SCRIPT.parent / preds_path).resolve()
    y_pred = pd.read_csv(preds_path).values.ravel()
    return y_pred


if __name__ == "__main__":
    import argparse

    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--adjacency", required=True)
    ap.add_argument("--labels", default=None)
    ap.add_argument("--n_dimen", type=int, default=5)
    ap.add_argument("--G", type=int, default=20)
    ap.add_argument("--iter", type=int, default=50000)
    ap.add_argument("--burnin", type=int, default=5000)
    ap.add_argument("--thin", type=int, default=100)
    args = ap.parse_args()

    y_pred = run_lspcm(
        adjacency_csv=args.adjacency,
        n_dimen=args.n_dimen, G=args.G,
        iter=args.iter, burnin=args.burnin, thin=args.thin,
        labels_csv=args.labels,
    )
    print("Predicciones:", y_pred[:20], "...")
