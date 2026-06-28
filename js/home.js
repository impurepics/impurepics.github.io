// We want tags to be scrolled horizontally and signal that it can be scrolled
function initTagStrip() {
  const tagStrip = document.querySelector(".tag-strip .tags");

  if (!tagStrip) {
    return;
  }

  function updateScrollState() {
    const atEnd =
      tagStrip.scrollLeft + tagStrip.clientWidth >= tagStrip.scrollWidth - 1;
    tagStrip.parentElement.classList.toggle("at-scroll-end", atEnd);
  }

  tagStrip.addEventListener(
    "wheel",
    function (event) {
      if (Math.abs(event.deltaY) <= Math.abs(event.deltaX)) {
        return;
      }

      const before = tagStrip.scrollLeft;
      tagStrip.scrollLeft += event.deltaY;

      if (tagStrip.scrollLeft !== before) {
        event.preventDefault();
      }
    },
    { passive: false },
  );

  tagStrip.addEventListener("scroll", updateScrollState);
  window.addEventListener("resize", updateScrollState);
  updateScrollState();
}

function initFeaturedPic() {
  const feature = document.querySelector(".featured-pic");
  const template = document.getElementById("featured-pics");

  if (!feature || !template) {
    return;
  }

  const choices = Array.from(template.content.querySelectorAll(".featured-pic"));

  if (choices.length === 0) {
    return;
  }

  const choice = choices[Math.floor(Math.random() * choices.length)];
  feature.replaceWith(choice.cloneNode(true));
}

initTagStrip();
initFeaturedPic();
