---
title: TAICA CVPDL 2025 HW1 — YOLOv10 Hard-Mining Pipeline

---

# TAICA CVPDL 2025 HW1 — YOLOv10 Hard-Mining Pipeline

This repository provides a **complete end-to-end pipeline** for the TAICA CVPDL 2025 HW1 pig-detection task, including:
- One-click EDA (data inspection & visualization)
- Preprocessing (black-border cropping / gamma / unsharp / letterbox)
- Background-only synthesis via Gaussian blur
- YOLO-format dataset generation
- Training from scratch with YOLOv10l
- Hard mining (TP/FP/FN bucketing)
- Targeted augmentation with Albumentations
- Two-stage fine-tuning (retain general, learn hard)
- Final inference and `submission.csv` export

> Recommended environment: **Google Colab + GPU**  
> Also supports local execution (ensure CUDA & PyTorch compatibility)
> 使用colab執行時要順跑請把權重那邊改為yolo訓練完權重
---

##  Environment Setup

###  Colab (recommended)
```bash
pip install -q -r requirements.txt