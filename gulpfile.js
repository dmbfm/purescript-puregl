const gulp = require("gulp");
const purescript = require("gulp-purescript");
const clean = require("gulp-clean");
const rollup = require("rollup-stream");
const source = require("vinyl-source-stream");
const purs = require("rollup-plugin-purs");
const babel = require("gulp-babel");
const browserSync = require("browser-sync").create();
const exampleModules = require("./examples/examples.json");

const examples = Object.keys(exampleModules);

const sources = [
  ...examples.map(v => "examples/" + v + "/**/*.purs"),
  "src/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs",
];

gulp.task("js", () => {
  gulp.src("js/**/*.js")
    .pipe(babel())
    .pipe(gulp.dest("./src"));
});

gulp.task("purs", ["js"], function () {
  return purescript.compile({
    src: sources,
    sourceMaps: true
  });
});

gulp.task("purs-clean", function () {
  return gulp
    .src("output", { read: false })
    .pipe(clean());  
});



gulp.task("examples-clean", function () {
  return gulp
    .src("examples/dist/*", { read: false })
    .pipe(clean());
});

gulp.task("clean", ["purs-clean", "examples-clean"]);

examples.forEach(
  name => {
    gulp.task("example-rollup:" + name, ["purs"], () => {
      return rollup({
        entry: "examples/" + name + "/Main.purs",
        format: "iife",
        sourceMap: true,
        plugins: [
          purs()
        ]
      })
        
        .pipe(source(name + ".bundle.js"))
      
        .pipe(gulp.dest('examples/dist'));

    });
  }
);

examples.forEach(
  name => {
    gulp.task("example-bundle:" + name, ["purs"], () => {
      return purescript.bundle({
        src: "output/**/*.js",
        module: exampleModules[name],
        output: "examples/dist/" + name + ".bundle.js",
        main: exampleModules[name]
      });
    });
  }
);

gulp.task("purs-watch", ["purs"], () => {
  gulp.watch([
    "src/**/*.purs",
    "js/**/*.js"
  ], ["purs"]);
});

gulp.task("examples-rollup", examples.map(e => "example-rollup:" + e));
gulp.task("examples-bundle", examples.map(e => "example-bundle:" + e));

gulp.task("build", ["purs", "examples-bundle"]);
gulp.task("build-rollup", ["purs", "examples-rollup"]);

gulp.task("default", ["build"]);

const mkDevTask = rollup =>
  gulp.task(rollup ? "dev-rollup" : "dev", [rollup ? "build-rollup" : "build"], () => {

    browserSync.init({
      server: {
        injectChanges: true,
        baseDir: "examples",
        directory: true,
        index: "index.html",
        files: "examples/dist/*"
      }
    });

    examples.forEach(
      e => {
        gulp.watch(
          [
            "src/**/*.purs",
            "js/**/*.js",
            "examples/" + e + "/**/*.purs",
            "examples/" + e + "/**/*.js",
          ],
          ["purs", (rollup ? "example-rollup:" : "example-bundle:") + e]
        );

        gulp.watch("examples/dist/**/*.js").on('change', () => { console.log("Ahh"); browserSync.reload(); });

      }
    );

  });

mkDevTask(true);
mkDevTask(false);
