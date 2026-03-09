---
title: Long-Tailed Object Detection (TAICA/CVPDL) – Reproducible Pipeline

---

# Long-Tailed Object Detection (TAICA/CVPDL) – Reproducible Pipeline

本專案實作 UAV 長尾四類別偵測（car/hov/person/motorcycle）的 **資料導向**流程：
1) 標註清理與格式轉換（pixels → YOLO）  
2) 小物件導向的 **離線增強**（ASRZ / QRM / RCPB / HNT）  
3) 分割與少數類 **適度重抽樣**  
4) **YOLO11s from scratch** 訓練（不使用預訓練）  
5) 推論與 **競賽制式 CSV** 產生



---

## 1. 環境安裝(建議使用colab)

### 1.1 Python 版本
建議 Python 3.10～3.11（其他版本亦可，但請注意 torch/ultralytics 相容性）。

### 1.2 安裝套件
```bash
# 建議在虛擬環境中執行
pip install -r requirements.txt
