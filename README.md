# Deep Clustering Suite

Empaquetado de 6 métodos de clustering profundo/no paramétrico para probarlos
con tus propios datos, con una interfaz de datos y métricas comunes.

**5 de los 6 corren en un único entorno PyTorch (`dcs-pytorch`).** El sexto
(LSPCM) es un muestreador MCMC Bayesiano, no una red neuronal, y sigue
usando el código R real de los autores -- pero se dispara desde el mismo
pipeline Python vía un puente por subprocess, así que en la práctica operas
todo desde un mismo lugar.

| Modelo | Repo | Framework | Estado |
|---|---|---|---|
| DEC | [vlukiyanov/pt-dec](https://github.com/vlukiyanov/pt-dec) | PyTorch | esqueleto, falta pegamento |
| DSC-Net | [XifengGuo/DSC-Net](https://github.com/XifengGuo/DSC-Net) | PyTorch | esqueleto, falta pegamento |
| VaDE | [ysterin/VaDE](https://github.com/ysterin/VaDE) | PyTorch Lightning | esqueleto, falta pegamento |
| DeepDPM | [BGU-CS-VIL/DeepDPM](https://github.com/BGU-CS-VIL/DeepDPM) (oficial) | PyTorch | esqueleto, falta pegamento |
| SB-VAE | reimplementación propia (`models/sbvae/pytorch_sbvae/`) | **PyTorch puro** | ✅ **funcional** |
| LSPCM | [gwee95/lspm](https://gitlab.com/gwee95/lspm) (oficial) | R (vía puente Python) | ✅ **funcional** |

## Por qué SB-VAE y LSPCM se trataron distinto

- **SB-VAE**: el código original de los autores está en Theano
  (descontinuado). Es una red neuronal de verdad, así que reescribirla en
  PyTorch moderno (usando `torch.distributions.Kumaraswamy` para el
  stick-breaking reparametrizado) tenía sentido y no arriesgaba nada: quedó
  en `models/sbvae/pytorch_sbvae/model.py`, probada de punta a punta con
  datos sintéticos (ACC=1.0 recuperando 4 clusters gaussianos).

- **LSPCM**: NO es una red neuronal, es un muestreador MCMC Bayesiano
  (Metropolis-within-Gibbs) con ~300 líneas de actualizaciones estadísticas
  hechas a mano por los autores (Gibbs, Metropolis-Hastings adaptativo,
  rotación Procrustes, cambio dinámico de dimensionalidad). Traducirlo a
  PyTorch no aporta velocidad (no hay gradientes ni GPU que aprovechar aquí)
  y sí arriesga bugs sutiles imposibles de detectar sin comparar
  exhaustivamente contra el código validado de los autores. Por eso se
  mantiene en R real, pero invocable desde Python vía
  `models/lspcm_bridge/run_lspcm_bridge.py` (subprocess + parseo de
  resultados), para que no tengas que salir de tu flujo de trabajo.

## Estructura

```
deep-clustering-suite/
├── data/                          # tus datos van aquí
├── common/
│   ├── data_loader.py             # carga .npy/.csv/imágenes/redes
│   ├── metrics.py                 # ACC, NMI, ARI (mismo criterio p/ todos)
│   └── run_all.py                 # plan de comandos + compara resultados
├── models/
│   ├── dec/{run_dec.py, dec_src/}
│   ├── dscnet/{run_dscnet.py, dscnet_src/}
│   ├── vade/{run_vade.py, vade_src/}
│   ├── deepdpm/{run_deepdpm.py, deepdpm_src/}
│   ├── sbvae/{run_sbvae.py, pytorch_sbvae/}      # ✅ PyTorch puro, funcional
│   ├── lspcm/{run_lspcm.R, lspcm_src/}           # código R real (autores)
│   └── lspcm_bridge/run_lspcm_bridge.py          # ✅ puente Python -> R
├── envs/
│   ├── environment_pytorch.yml    # ÚNICO entorno para 5 de los 6 modelos
│   └── environment_r.txt          # instrucciones para instalar `lspm` en R
└── results/                       # cada modelo guarda aquí preds.npy/.csv
```

## Setup rápido

```bash
# Entorno único para DEC, DSC-Net, VaDE, DeepDPM, SB-VAE
conda env create -f envs/environment_pytorch.yml
conda activate dcs-pytorch

# (Opcional, solo si tienes datos de red) Instalar R + paquete lspm
# El código ya está en models/lspcm_src/, ver envs/environment_r.txt
# para los comandos exactos de instalación.
```

## Cómo probar con tus datos

1. Copia tus datos a `data/` (vectores `.npy`/`.csv`, imágenes en carpetas,
   o matriz de adyacencia para LSPCM).
2. `python common/run_all.py plan --data data/tu_archivo --labels
   data/labels.npy --n_clusters K` para ver el comando exacto de cada modelo.
3. Ejecuta cada modelo (todos desde `dcs-pytorch`, incluido LSPCM vía el
   puente).
4. `python common/run_all.py compare --labels data/labels.npy` para ver
   ACC/NMI/ARI de todos lado a lado.

## Estado actual de la integración

**SB-VAE y LSPCM: ✅ funcionales de punta a punta**, probados con datos
sintéticos.

**DEC, DSC-Net, VaDE, DeepDPM: esqueleto listo, falta el "pegamento"
final.** Cada `run_<modelo>.py` ya carga tus datos con el loader común y
documenta exactamente qué API del repo hay que llamar, pero marca con
`NotImplementedError` el punto donde falta conectar esa API con tus datos
-- porque cada repo tiene su propio formato de entrada interno (Dataset de
PyTorch, DataModule de Lightning, tensores `.pt`, `.mat`, etc.) y eso
depende de la forma real de tus datos, que todavía no conozco.

**Cuando quieras, dime:**
1. Con qué modelo seguir (recomiendo DeepDPM: CLI oficial lista, corre en
   CPU sin drama).
2. La forma real de tus datos (¿cuántas muestras, features/dimensiones,
   tienes labels reales para evaluar?).

Y completo la integración de ese modelo específico igual que ya hice con
SB-VAE y LSPCM, dejándolo corriendo de punta a punta con un ejemplo real.

## Notas de rendimiento (CPU-only)

- **DSC-Net**: la capa self-expressive es O(N²) — limita a pocos cientos de
  muestras o será muy lento/pesado en memoria.
- **SB-VAE**: entrenable en CPU sin problema en datasets medianos.
- **LSPCM**: es MCMC, no gradiente — `iter`/`burnin`/`thin` son iteraciones
  de cadena, no épocas. Con los valores por defecto del paper (iter=5e4)
  puede tardar varios minutos incluso en redes pequeñas. Para una prueba
  rápida, usa `--iter 5000 --thin 20`.
- **VaDE / DEC / DeepDPM**: entrenables en CPU sin problema en datasets
  medianos (miles de muestras), solo más lento que con GPU.
