
function mulM3ArrayNew(m1, m2) {
  return [m1[0] * m2[0] + m1[1] * m2[3] + m1[2] * m2[6]
    , m1[0] * m2[1] + m1[1] * m2[4] + m1[2] * m2[7]
    , m1[0] * m2[2] + m1[1] * m2[5] + m1[2] * m2[8]

    , m1[3] * m2[0] + m1[4] * m2[3] + m1[5] * m2[6]
    , m1[3] * m2[1] + m1[4] * m2[4] + m1[5] * m2[7]
    , m1[3] * m2[2] + m1[4] * m2[5] + m1[5] * m2[8]

    , m1[6] * m2[0] + m1[7] * m2[3] + m1[8] * m2[6]
    , m1[6] * m2[1] + m1[7] * m2[4] + m1[8] * m2[7]
    , m1[6] * m2[2] + m1[7] * m2[5] + m1[8] * m2[8]
  ];
}

function mulM3ArrayOut(m1, m2, out) {
  out[0] = m1[0] * m2[0] + m1[1] * m2[3] + m1[2] * m2[6]
  out[1] = m1[0] * m2[1] + m1[1] * m2[4] + m1[2] * m2[7]
  out[2] = m1[0] * m2[2] + m1[1] * m2[5] + m1[2] * m2[8]
  out[3] = m1[3] * m2[0] + m1[4] * m2[3] + m1[5] * m2[6]
  out[4] = m1[3] * m2[1] + m1[4] * m2[4] + m1[5] * m2[7]
  out[5] = m1[3] * m2[2] + m1[4] * m2[5] + m1[5] * m2[8]
  out[6] = m1[6] * m2[0] + m1[7] * m2[3] + m1[8] * m2[6]
  out[7] = m1[6] * m2[1] + m1[7] * m2[4] + m1[8] * m2[7]
  out[8] = m1[6] * m2[2] + m1[7] * m2[5] + m1[8] * m2[8]  
}

function mulM3Float32ArrayOut(m1, m2, out) {
  out[0] = m1[0] * m2[0] + m1[1] * m2[3] + m1[2] * m2[6]
  out[1] = m1[0] * m2[1] + m1[1] * m2[4] + m1[2] * m2[7]
  out[2] = m1[0] * m2[2] + m1[1] * m2[5] + m1[2] * m2[8]
  out[3] = m1[3] * m2[0] + m1[4] * m2[3] + m1[5] * m2[6]
  out[4] = m1[3] * m2[1] + m1[4] * m2[4] + m1[5] * m2[7]
  out[5] = m1[3] * m2[2] + m1[4] * m2[5] + m1[5] * m2[8]
  out[6] = m1[6] * m2[0] + m1[7] * m2[3] + m1[8] * m2[6]
  out[7] = m1[6] * m2[1] + m1[7] * m2[4] + m1[8] * m2[7]
  out[8] = m1[6] * m2[2] + m1[7] * m2[5] + m1[8] * m2[8]
}

function mulM3Float32ArrayNew(m1, m2) {
  return new Float32Array([m1[0] * m2[0] + m1[1] * m2[3] + m1[2] * m2[6]
    , m1[0] * m2[1] + m1[1] * m2[4] + m1[2] * m2[7]
    , m1[0] * m2[2] + m1[1] * m2[5] + m1[2] * m2[8]

    , m1[3] * m2[0] + m1[4] * m2[3] + m1[5] * m2[6]
    , m1[3] * m2[1] + m1[4] * m2[4] + m1[5] * m2[7]
    , m1[3] * m2[2] + m1[4] * m2[5] + m1[5] * m2[8]

    , m1[6] * m2[0] + m1[7] * m2[3] + m1[8] * m2[6]
    , m1[6] * m2[1] + m1[7] * m2[4] + m1[8] * m2[7]
    , m1[6] * m2[2] + m1[7] * m2[5] + m1[8] * m2[8]
  ]);
}

exports.vArrAdd = function (v1) {
  return function (v2) {
    return [
      v1[0] + v2[0],
      v1[1] + v2[1],
      v1[2] + v2[2],
    ];
  }
}  

exports.runTests = function (a) {
  return function () {

    var
      addV = a.addV,
      addFV = a.addFV,
      mulFM3 = a.mulFM3,
      mulM3 = a.mulM3,
      vArrAdd = a.vArrAdd;


    var x = new Float32Array([1.0, 2.0, 3.0, 4.0]);
    var y = new Float32Array([1.0, 2.0, 3.0, 4.0]);

    var v1 = { x: 1.0, y: 2.0, z: 3.0, w: 4.0 };
    var v2 = { x: 1.0, y: 2.0, z: 3.0, w: 4.0 };

    var u1 = [1.0, 2.0, 3.0];
    var u2 = [1.0, 2.0, 3.0];
    

    var m1 = [
      1.0, 2.0, 3.0,
      4.0, 5.0, 6.0,
      7.0, 8.0, 9.0
    ];

    var m2 = [
      4.0, 5.0, 6.0,
      1.0, 2.0, 3.0,
      7.0, 8.0, 9.0
    ];

    var m3 = new Array(9);

    var fm1 = new Float32Array(m1);
    var fm2 = new Float32Array(m2);
    var fm3 = new Float32Array(m1);

    var pdiff = function (a, b) {
      return ((a - b) / a) * 100;
    }

    window.addEventListener("DOMContentLoaded", function () {



      new Benchmark.Suite()

        .add("FVector4 Add", function () {
          addFV(x)(y)();
        })

        .add("Vector4 Add", function () {
          addV(v1)(v2);
        })

        .add("Vector Array Add", function () {
          vArrAdd(u1)(u2);
        })

        .on('complete', function () {
          var fastestHz = this.filter('fastest')[0].hz;
          var lastHz = fastestHz;

          _.sortBy(this, [function (o) { return -o.hz; }]).forEach(
            function (element) {

              console.log(
                element.name,
                element.hz,
                "(" + pdiff(fastestHz, element.hz) + "%)",
                "(" + pdiff(lastHz, element.hz) + "%)"
              );

              lastHz = element.hz;

            }, this);

        })

        .run();

      new Benchmark.Suite()

        .add("FMatrix3 mul", function () {
          mulFM3(fm1)(fm2)(fm3)();
        })

        .add("Matrix3 mul", function () {
          mulM3(m1)(m2);
        })

        .add("mulM3ArrayNew", function () {
          mulM3ArrayNew(m1, m2);
        })

        .add("mulM3ArrayOut", function () {
          mulM3ArrayOut(m1, m2, m3);
        })

        .add("mulM3Float32ArrayNew", function () {
          mulM3Float32ArrayNew(fm1, fm2);
        })

        .add("mulM3Float32ArrayOut", function () {
          mulM3Float32ArrayOut(fm1, fm2, fm3);
        })

        .on('complete', function () {          

          var fastestHz = this.filter('fastest')[0].hz;
          var lastHz = fastestHz;          

          _.sortBy(this, [function (o) { return -o.hz; }]).forEach(
            function (element) {

              console.log(
                element.name,
                element.hz,
                "(" + pdiff(fastestHz, element.hz) + "%)",
                "(" + pdiff(lastHz, element.hz) + "%)"
              );

              lastHz = element.hz;

          }, this);


        }).run();

    }); 
  }  
}