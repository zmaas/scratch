'''MNIST test using keras'''
from keras.datasets import mnist
from keras.utils import to_categorical
from keras import models
from keras import layers

# Load data
(train_images, train_labels), (test_images, test_labels) = mnist.load_data()


# print("Train")
# print(train_images.shape)
# print(len(train_labels))
# print(train_labels)
# print("Test")
# print(test_images.shape)
# print(len(test_labels))
# print(test_labels)

# Define our neural network
network = models.Sequential()
network.add(layers.Dense(512, activation='relu', input_shape=(28 * 28, )))
network.add(layers.Dense(10, activation='softmax'))

# Compile the network
network.compile(
    optimizer='rmsprop', loss='categorical_crossentropy', metrics=['accuracy'])

# Reshape data for training
train_images = train_images.reshape((60000, 28 * 28))
train_images = train_images.astype('float32') / 255

# Reshape data for testing
test_images = test_images.reshape((10000, 28 * 28))
test_images = test_images.astype('float32') / 255

# Change labels to categorical for the model
train_labels = to_categorical(train_labels)
test_labels = to_categorical(test_labels)

# Actually train the model
network.fit(train_images, train_labels, epochs=5, batch_size=128)

# Test the model
test_loss, test_acc = network.evaluate(test_images, test_labels)
print("Accuracy: ", test_acc)
