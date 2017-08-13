const rollup = require('rollup');
const pathExists = require('path-exists');
const path = require('path');
const purs = require('rollup-plugin-purs');
const watch = require('watch');
const argv = require('minimist')(process.argv.slice(2));
const fs = require('fs');
const fe = require('file-extension');
const spawn = require('cross-spawn');

//const compileCommand = name => "pulp build -I examples/ -- --source-maps" + name;

const compileCommand = "pulp"
const compileArgs = name => ['build', '-I', 'examples/' + name, '--', '--source-maps'];

const buildExample = (name, w) => {
  if (!name) {
    console.log("No example name supplied");
    return;
  }

  let entry = path.join('.', 'examples', name, 'Main.purs');
  let bundle = path.join('.', 'examples', 'dist', name + '.bundle.js');

  if (!pathExists.sync(entry)) {
    console.log("Exemple '" + name + "' not found.");
    return;
  }

  let cache = null;
  let lock = false;

  if (w) {

    const onChange = () => {
      if (!lock) {
        lock = true;
        const command = spawn(compileCommand, compileArgs(name));
        command.stderr.on('data', data => {
          console.log("[pulp]:", data.toString());
        });
        command.stdout.on('data', data => {
          console.log("[pulp]:", data.toString());
        });
        command.on('close', x => {
          buildWithCache(cache);
        })
      }
    }

    watch.watchTree(path.join('.', 'src'), { interval: 2 }, (f, curr, prev) => {
      if (typeof f == "object" && prev === null && curr === null) {
      } else {
        onChange();
      }
    });

    watch.watchTree(path.join('.', 'examples', name), { interval: 2, filter: f => fe(f) == 'purs' }, (f, curr, prev) => {
      if (typeof f == "object" && prev === null && curr === null) {
      } else {
        onChange();
      }
    });

  }

  const buildWithCache = c => {

    lock = true;

    rollup.rollup({
      entry: entry,
      cache: c,
      plugins: [
        purs()
      ]
    }).then(b => {
      cache = b;
      b.write({
        dest: bundle, format: "iife", sourceMap: true
      }).then(() => {
        lock = false;
      });
    });
  }

  lock = true;

  const command = spawn(compileCommand, compileArgs(name));
  command.stderr.on('data', data => {
    console.log("[pulp]:", data.toString());
  });
  command.stdout.on('data', data => {
    console.log("[pulp]:", data.toString());
  });
  command.on('close', x => {
    buildWithCache(cache);
  });

  //buildWithCache();

}

const buildAllExamples = () => {
  const dirs = p => fs.readdirSync(p).filter(file => fs.lstatSync(path.join(p, file)).isDirectory() && file != "dist");
  const examples = dirs(path.join('.', 'examples'));
  examples.forEach(e => buildExample(e, false));
}

console.dir(argv);
switch (argv._[0]) {
  case "example":  
    buildExample(argv._[1], argv.w || argv.watch);
    break;

  case "examples":
    buildAllExamples();
    break;

  default:
    console.log("Invalid command");
    break;
}
