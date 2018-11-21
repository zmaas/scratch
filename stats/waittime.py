import numpy as np

# From https://jakevdp.github.io/blog/2018/09/13/waiting-time-paradox/

N = 1000000 # Number of trials
tau = 10 # Time between busses
rand = np.random.RandomState(42) # Random seed

# Calculate arrivals
bus_arrival_times = N * tau * np.sort(rand.rand(N))

intervals = np.diff(bus_arrival_times)
print("Mean: ", intervals.mean())

def simulate_wait_times(arrival_times,
                        rseed=8675309,  # Passenger Seed
                        n_passengers=1000000):
    rand = np.random.RandomState(rseed)

    arrival_times = np.asarray(arrival_times)
    passenger_times = arrival_times.max() * rand.rand(n_passengers)

    # find the index of the next bus for each simulated passenger
    i = np.searchsorted(arrival_times, passenger_times, side='right')

    return arrival_times[i] - passenger_times

wait_times = simulate_wait_times(bus_arrival_times)
print("Mean wait time: ", wait_times.mean())

