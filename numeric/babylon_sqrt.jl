function m_sqrt(num)
    guess = num / 2
    tmp = 0
    while true
        tmp = 0.5 * (guess + (num / guess))
        if tmp != guess
            guess = tmp
        else
            return guess
        end
    end
end

print(m_sqrt(10), '\n')
