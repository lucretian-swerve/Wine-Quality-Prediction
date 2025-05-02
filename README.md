# 🍷 Wine Quality Prediction

This project uses machine learning techniques to predict wine quality based on physicochemical features. Implemented in R, it compares the performance of a logistic regression model and a custom-built neural network on a real-world dataset.

---

## 📂 Overview

- Predict wine quality from 11 numerical features (e.g., pH, alcohol, acidity).
- Compare logistic regression and a neural network with one hidden layer.
- Evaluate model accuracy and performance using confusion matrices and visualizations.

---

## 📊 Dataset

- **Source**: [UCI Machine Learning Repository – Wine Quality](https://archive.ics.uci.edu/ml/datasets/wine+quality)
- **Size**: 1,599 red wine samples
- **Target**: Wine quality score (integer 0–10, typically 3–8)

---

## 🧠 Models Implemented

### Logistic Regression
- Baseline model to establish a performance benchmark.

### Neural Network
- One hidden layer with sigmoid activation.
- Output layer uses sigmoid activation for binary classification or softmax (if multiclass).
- Custom implementation in R, trained using backpropagation.

---

## ⚙️ Technologies Used

- R (base R + `caret`, `nnet`, `ggplot2`)
- Data preprocessing (normalization, train/test split)
- Custom neural network functions

---

## 📈 Results

| Model              | Accuracy | RMSE   |
|-------------------|----------|--------|
| Logistic Regression | 74.5%    | 0.52   |
| Neural Network      | 82.1%    | 0.43   |

> ✅ The neural network consistently outperformed logistic regression on classification accuracy and mean error, particularly for mid-range wine quality scores.

---

## 📁 Repository Structure

## 📌 Key Takeaways

- Demonstrates end-to-end ML workflow in R.
- Shows ability to build and train neural networks from scratch.
- Highlights model evaluation and interpretation skills.

---

## 💡 Next Steps

- Explore hyperparameter tuning (e.g., hidden layer size, learning rate).
- Try a multiclass classifier (softmax) instead of binarizing wine quality.
- Compare with other models (Random Forest, SVM, XGBoost).

---

## 👤 Author

William Kinum  
Master’s in Applied Mathematics, Johns Hopkins University  
📧 [williamkinum@gmail.com]
