const answersNet =
  [ "traverse"
  , "The answer is probably traverse, because it always is"
  , "traverse should do the trick"
  , "The answer is always traverse/sequence"
  , "try traverse"
  , "I'm not sure if everyone will like it, but you can also use traverse"
  , "you might need a traverse though"
  , "yeah I guess traverse is the more straightforward thing to do here"
  , "No longer needed. Just use traverse/sequence"
  , "if it's always traverse why not start with that?"
  , "traverse is another way to do it"
  , "can't do what you want with traverse?"
  , "in fact, I think you can possibly just do a traverse"
  , "Since it could be more general, I would just do traverse"
  , "You can take a traverse"
  , "sequence or traverse"
  , "you should just be able to use traverse"
  , "there must be some way of using traverse here"
  , "it’s something with traverse, I don’t remember exactly what, sorry"
  , "I'm guessing you're looking for traverse sequence."
  , "Sounds like you might want traverse"
  , "Pretty sure you want traverse"
].map(s => s.replace("traverse", "<code>traverse</code>"))

const answerBox = document.getElementById("fp-bot-answer");
const submitButton = document.getElementById("fp-bot-submit");
submitButton.addEventListener("click", answer);

function answer(){
  const output = answersNet[Math.floor(Math.random() * answersNet.length)];
  answerBox.innerHTML = `<p>${output}</p>`;
};
