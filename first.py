# Step 1: Handle Command Line

if __name__ == "__main__":
    print("Simple script to plot x and y values.")

    # Step 2: Fill Lists
    import math
    xval = []
    yval = []

    for x in range(-50, 51):
        xval.append(x * 0.1)

    for x in xval:
        yval.append(math.sin(x))

    # Step 3: Plot Lists
    plt.plot(xval, yval, label='y = sin(x)')
    plt.title('Plot of y = f(x)')
    plt.xlabel('x values')
    plt.ylabel('y values')
    plt.legend()
    plt.grid(True)
    plt.show()

