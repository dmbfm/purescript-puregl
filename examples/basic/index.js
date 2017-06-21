
if (module.hot) {
  console.log(module.hot);
  module.hot.accept();
}

require('./Main.purs').main();
