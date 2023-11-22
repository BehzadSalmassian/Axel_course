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

    print("xval:", xval)
    print("yval:", yval)
