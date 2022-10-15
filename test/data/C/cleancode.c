import std.stdio, std.algorithm, std.range;

// generates all primes in [start,end) and returns all primes in [0,end).
// primes array must contain all primes < start
uint[] grow_sieve(uint[] primes, uint start, uint end) {
    assert(end >= start && start > 1);
    bool[] composite;
    composite.length = end - start;

    // mask multiples of existing primes over the expanded range
    foreach (p; primes) {
        // start at first multiple of p in [start,end)
        for (uint j = p * (((start - 1) / p) + 1); j < end; j += p) {
            composite[j - start] = 1;
        }
    }

    // locate primes inside the expanded range
    foreach (i; start..end) {
        if (composite[i - start]) continue;
        for (uint j = i * 2; j < end; j += i) {
            composite[j - start] = 1;
        }
    }

    // concatenate all numbers in [start, end) that aren't marked composite
    return primes ~ iota(start,end).filter!(a => !composite[a - start]).array;
}

// Generate first n primes by successive applications of the 
// Sieve of Eratosthenes
uint[] generate_primes(uint n) {
    const STEP_SIZE = 10_000_000;
    uint[] primes = [ ];
    uint start = 2;
    for (uint end = STEP_SIZE; primes.length < n; end += STEP_SIZE) {
        primes = grow_sieve(primes, start, end);
        start = end;
    }
    return primes[0..n];
}