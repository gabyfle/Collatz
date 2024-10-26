import pandas as pd
import matplotlib.pyplot as plt


def plot_func(x, y, title, x_label, y_label, save_path="func.svg"):
    """
    Plots a function given that x and y = f(x)
    """
    plt.figure(figsize=(12, 6))
    # Make a log scale for x axis
    plt.xscale("log")
    plt.plot(x, y, color="green")
    plt.title(title)
    plt.xlabel(x_label)
    plt.ylabel(y_label)

    plt.savefig(save_path)


def plot_hist(x, title, x_label, y_label, save_path="hist.svg", res=700):
    """
    Plots an histogram of x
    """
    plt.figure(figsize=(12, 6))
    plt.hist(x, bins=res, color="green", density=True)
    plt.title(title)
    plt.xlabel(x_label)
    plt.ylabel(y_label)

    plt.savefig(save_path)


data = pd.read_csv("dataset.csv")
x = data["n"]
y = data["steps"]

# plot_func(x, y, "C(N)", "N", "Steps", "steps.svg")
plot_hist(y, "Histogram of steps", "Steps", "Frequency", "hist.svg")
