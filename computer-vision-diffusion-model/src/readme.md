---
title: FiLM-UNet Diffusion on MNIST (Colab/L4)

---

# FiLM-UNet Diffusion on MNIST (Colab/L4)

This repo reproduces a FiLM-UNet with SDPA attention for DDPM/DDIM generation on
RGB MNIST (28×28×3), including EMA, MinSNR loss, FID evaluation and a diffusion
trajectory visualization.

- **Hardware**: Google Colab + GPU (tested on **NVIDIA L4**)
- **Outputs**
  - Generated images: `/content/artifacts/samples/*.png` (10,000 by default)
  - Diffusion trajectory: `/content/artifacts/vis/denoise_trajectory_8x8.png`
  - Optional checkpoints: `/content/ddpm_film_attn_epoch_XX.pth`

---

## 1. Colab Setup

1. **Runtime → Change runtime type → GPU**（選擇 L4/自動）
2. 在第一格安裝套件：
   ```bash
   pip install -q -U -r requirements.txt
