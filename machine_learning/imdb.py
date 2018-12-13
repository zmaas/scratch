'''IMDB Sentiment Classification'''
from keras.datasets import imdb
from keras import layers
from keras import models
import matplotlib.pyplot as plt
import numpy as np

(train_data, train_labels), (test_data,
                             test_labels) = imdb.load_data(num_words=2500)


def vectorize_sequences(sequences, dimension=2500):
    results = np.zeros((len(sequences), dimension))
    for i, sequence in enumerate(sequences):
        results[i, sequence] = 1
    return results


x_train = vectorize_sequences(train_data)
x_test = vectorize_sequences(test_data)

y_train = np.asarray(train_labels).astype('float32')
y_test = np.asarray(test_labels).astype('float32')

model = models.Sequential()
model.add(layers.Dense(16, activation='relu', input_shape=(2500, )))
model.add(layers.Dense(16, activation='relu'))
model.add(layers.Dense(1, activation='sigmoid'))

model.compile(
    optimizer='rmsprop', loss='binary_crossentropy', metrics=['accuracy'])

x_val = x_train[:2500]
partial_xtrain = x_train[2500:]

y_val = y_train[:2500]
partial_ytrain = y_train[2500:]

model.fit(
    partial_xtrain,
    partial_ytrain,
    epochs=4,
    batch_size=512,
    validation_data=(x_val, y_val))

results=model.evaluate(x_test,y_test)
