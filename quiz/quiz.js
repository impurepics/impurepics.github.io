const quizContainer = document.getElementById("quiz");
const resultsContainer = document.getElementById("quiz-results");
const submitButton = document.getElementById("quiz-submit");

const pictures =
  { "Semigroup": "https://imgur.com/2aL4la7"
  , "Monoid": "https://imgur.com/HDKHYmz"
  , "Functor": "https://imgur.com/Qfre8mA"
  , "Foldable": "https://imgur.com/XmkWWAC"
  , "Traversable": "https://imgur.com/g4WMehg"
  , "Applicative": "https://imgur.com/CsHPmJi"
  , "Monad": "https://imgur.com/kmIQF7x"
  };

// What are you looking at?
const questions = [
  {
    question: "Hi. What's your name?",
    answers: {
      a: "Oleg",
      b: "Emily",
      c: "Simon",
      d: "None of the above"
    },
    hits: ["a", "b", "c"],
    fees: []
  },
  {
    question: "Functions are",
    answers: {
      a: "Total, Deterministic, Pure",
      b: "Total, Lazy, Pure",
      c: "Complete, Deterministic, Pure",
      d: "Total, Lazy, Free"
    },
    hits: ["a"],
    fees: []
  },
  {
    question: "Should Haskell be used in production?",
    answers: {
      a: "No, it's not production ready",
      b: "Maybe, just for tooling",
      c: "Yes",
      d: "Yes, and I'm already using it in production"
    },
    hits: ["c", "d"],
    fees: []
  },
  {
    question: "Which one is not a Haskell Extension?",
    answers: {
      a: "DeriveAnyClass",
      b: "DeriveGeneric",
      c: "DerivingNewtype",
      d: "DerivingVia"
    },
    hits: ["c"],
    fees: []
  },
  {
    question: "Which one is not a Monad law?",
    answers: {
      a: "Left identity",
      b: "Right identity",
      c: "Commutativity",
      d: "Associativity"
    },
    hits: ["c"],
    fees: []
  },
  {
    question: "Have you written any Monad tutorials?",
    answers: {
      a: "No",
      b: "No, but I would like to",
      c: "Yes, and it was better than any other monad tutorial",
      d: "Yes, as part of the full fp course/book"
    },
    hits: ["d"],
    fees: ["c"]
  },
  {
    question: "Which function is missing?" + "<br>" +
              "<code>mySum = ??? (+) 0 [1..100000000]</code>",
    answers: {
      a: "<code>foldl</code>",
      b: "<code>foldr</code>",
      c: "<code>foldl'</code>",
      d: "It doesn't matter"
    },
    hits: ["c"],
    fees: ["a"]
  }
];

buildQuiz();

const nextButton = document.getElementById("quiz-next");
const slides = document.querySelectorAll(".quiz-slide");
let currentSlide = 0;

showSlide(0);

submitButton.addEventListener("click", showResults);
nextButton.addEventListener("click", showNextSlide);

function buildQuiz(){
  const output = [];

  questions.forEach((currentQuestion, questionNumber) => {
    const answers = [];

    for (letter in currentQuestion.answers) {
      answers.push(
        `<label>
           <input type="radio" name="question${questionNumber}" value="${letter}">
           ${letter} :
           ${currentQuestion.answers[letter]}
         </label>`
      );
    }

    output.push(
      `<div class="quiz-slide">
         <div class="quiz-question"> ${currentQuestion.question} </div>
         <div class="quiz-answers"> ${answers.join("")} </div>
       </div>`
    );
  });

  quizContainer.innerHTML = output.join("");
}

function showResults() {
  const typeclasses = ["Semigroup", "Monoid", "Functor", "Foldable", "Traversable", "Applicative", "Monad"]
  const answerContainers = quizContainer.querySelectorAll(".quiz-answers");
  let score = 0;

  questions.forEach((currentQuestion, questionNumber) => {
    const answerContainer = answerContainers[questionNumber];
    const selector = "input[name=question" + questionNumber + "]:checked";
    const userAnswer = (answerContainer.querySelector(selector) || {}).value;

    if (currentQuestion.hits.includes(userAnswer)) {
      score++;
    } else if (currentQuestion.fees.includes(userAnswer)) {
      score--;
    }
  });

  const finalScore = Math.max(0, Math.min(score, 6));
  const you = typeclasses[finalScore];
  const picture = pictures[you];
  const article = new RegExp('^[aeiou].*', 'i').test(you) ? "an" : "a";
  const congrats = `Congratulations, you're ${article} ${you}!`;
  const share = `https://twitter.com/intent/tweet?text=${congrats}&url=${picture}&via=impurepics`;

  resultsContainer.innerHTML =
    `<section>
       <div class="post-body">
         <h3>${congrats}</h3>
         <p class="text-muted text-center">
             Don't forget to share it with your friends
         </p>
         <div>
           <a href="${share}">
             <i class="fa fa-twitter fa-2x fa-color"></i>
           </a>
         </div>
         <img src="${picture}.png" alt="result"/>
       </div>
     </section>`;

   quizContainer.style.display = "none";
   submitButton.style.display = "none";
}

function showSlide(n) {
  slides[currentSlide].classList.remove("quiz-active-slide");
  slides[n].classList.add("quiz-active-slide");
  currentSlide = n;
  if (currentSlide === slides.length-1) {
    nextButton.style.display = "none";
    submitButton.style.display = "inline-block";
  } else {
    nextButton.style.display = "inline-block";
    submitButton.style.display = "none";
  }
}

function showNextSlide() {
  showSlide(currentSlide + 1);
}
