using BenchmarkTools

function m_sqrt(num):
    guess = num / 2
    tmp = 0
    while True:
        tmp = 0.5 * (guess + (num / guess))
        if tmp != guess:
            guess = tmp
        else:
            return guess
        end
    end
end
