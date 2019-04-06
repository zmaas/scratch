# Toy Markov Chain

import asyncio as aio
from numpy import isclose
import random
import string


class MarkovChain:
    """Markov chain class implemented using a dictionary of dictionaries.
    We don't use a matrix because when dealing with text we expect the
    model to be extremely sparse. Dictionaries provide a significant
    space advantage here, and roughly O(1) access time for actually
    generating text from the model.
    """
    transitionMatrix = {}
    curr_state = 0
    # Number of previous words to consider. 2 to 3 seems to be the sweet spot
    sequence_length = 3

    def construct_model(self, filename):
        """Take in a plain text (ideally a utf8 file) and generate a markov
        chain for that text using the word frequencies to calculate
        transition probabilities. The number of words considered when
        transitioning is determined by the object parameter
        sequence_length, and should be greater than 2. Considering
        more words will typically give more accurate results, but may
        end up printing the original text if the parameter is set too large.
        """

        # Read in text and generate bigrams
        with open(filename) as corpus:
            # Convert to lowercase words
            full_text = corpus.read().translate(
                str.maketrans('', '', string.punctuation)).lower().split()
            # Calculate bigrams and indices
            n = self.sequence_length
            ngrams = zip(*[full_text[i:] for i in range(n)])
            ngramlist = [" ".join(ngram) for ngram in ngrams]
            # Use a generator comprehension here for performance on large input
            ngrampairs = ((x, y) for x, y in zip(ngramlist, ngramlist[1:]))

            # Generate counts from bigrams
            T = {}
            for pair in ngrampairs:
                if pair[0] not in T:
                    T[pair[0]] = {}
                if pair[1] not in T[pair[0]]:
                    T[pair[0]][pair[1]] = 1
                else:
                    T[pair[0]][pair[1]] += 1

            # Convert counts into a markov matrix
            for item in T:
                total = sum(T[item].values())
                T[item] = {k: v / total for k, v in T[item].items()}
            self.transitionMatrix = T
            self.curr_state = random.choice(list(self.transitionMatrix.keys()))

    def randomize_state(self):
        """Randomize the model's state, used  after a sentence is generated."""
        self.curr_state = random.choice(list(self.transitionMatrix.keys()))

    def transition(self):
        """Iterate the state of the markov chain."""
        guess = random.random()
        if self.curr_state in self.transitionMatrix:
            for key, prob in self.transitionMatrix[self.curr_state].items():
                guess -= prob
                if guess <= 0:
                    self.curr_state = key
                    break
        else:
            self.randomize_state()

    def verify_markov_chain(self):
        """Set the transition matrix with property verification."""
        for j in self.transitionMatrix:
            k = sum(self.transitionMatrix[j].values())
            passes_p = isclose(k, 1) or isclose(k, 0)
        return passes_p


class TextGenerator(MarkovChain):
    "Text generator implemented using a markov chain with 2-grams."

    # Whether to perform additional checks at the cost of performance
    debug = False
    # Generation Parameters for Sentences and Paragraphs
    sentence_min_length = 5
    sentence_max_length = 20
    punctuation_choices = [".", ".", ".", ".", "!"]

    async def describe(self):
        """Describe the current state of the model and the properties. This
        will also verify that the generated model is Markov, which can take
        some time for larger models.
        """
        print("TextGenerator Object based on a Markov Chain")
        print("Model states: ", len(self.transitionMatrix))
        print("Current state:", self.curr_state)
        if self.debug:
            print("[Debug] Running Tests...")
            passes_tests = self.verify_markov_chain()
            passes_predicate = "Passes Tests: "
            if passes_tests:
                print(passes_predicate, True)
            else:
                print(passes_predicate, False)

    def generate_word(self):
        """Generate a single word from the model by transitioning the model
        state and returning the new state.
        """
        self.transition()
        return self.curr_state

    async def generate_sentence(self, num_words):
        """Generate a sentence consisting of num_words many words, with the
        first letter capitalized and randomized punctuation at the end.
        """
        sentence = [self.generate_word().capitalize()]
        for _ in range(num_words):
            sentence.append(self.generate_word().split()[-1])
        self.randomize_state()
        return " ".join(sentence) + random.choice(self.punctuation_choices)

    async def generate_paragraph(self, num_sentences):
        """Generate a string of num_sentences many sentences, constructed
        using the generate_sentence method from above.
        """
        state = []
        for _ in range(num_sentences):
            state.append(await self.generate_sentence(
                random.randint(self.sentence_min_length,
                               self.sentence_max_length)))
        return ' '.join(state)


# Generate the model.
async def main():
    corpus_file = "mobydick.txt"
    g = TextGenerator()
    g.construct_model(corpus_file)
    # g.describe()
    output = ""
    for _ in range(10):
        output += await g.generate_paragraph(random.randint(7, 20))
    print(output)


aio.run(main())
