'''Reuters Newswire Classification'''

from keras.datasets import reuters
from keras.utils.np_utils import to_categorical
from xgboost import XGBRegressor
import numpy as np

(train_data, train_labels), (test_data,
                             test_labels) = reuters.load_data(num_words=10000)


def vectorize_sequences(sequences, dimension=10000):
    results = np.zeros((len(sequences), dimension))
    for i, sequence in enumerate(sequences):
        results[i, sequence] = 1
    return results


x_train = vectorize_sequences(train_data)
x_test = vectorize_sequences(test_data)
train_labels = to_categorical(train_labels)
test_labels = to_categorical(test_labels)

model = XGBRegressor()

x_val = x_train[:1000]
partial_xtrain = x_train[1000:]

y_val = train_labels[:1000]
partial_ytrain = train_labels[1000:]

print("Training model...")
model.fit(x_train, train_labels)
