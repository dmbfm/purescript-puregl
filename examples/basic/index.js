
let count = 0;

if (module.hot) {

  module.hot.accept();

  if (window['state']) {
    count = window['state'];
  }
  module.hot.dispose(() => { window['state'] = count });

} 

console.log("COUNT", count);

count++;

require('./Main.purs').main();
