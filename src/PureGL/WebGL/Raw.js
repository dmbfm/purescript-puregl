'use strict';

exports.nullBufferObject = null;
exports.nullVertexArrayObject = null;
exports.nullFramebufferObject = null;

// Buffers
exports.createBuffer = function (ctx) {
    return function () {
        return ctx.createBuffer();
    };
};
exports.bindBuffer = function (ctx) {
    return function (target) {
        return function (buffer) {
            return function () {
                ctx.bindBuffer(target, buffer);
            };
        };
    };
};
exports.bufferData = function (_) {
    return function (ctx) {
        return function (target) {
            return function (array) {
                return function (usage) {
                    return function () {
                        ctx.bufferData(target, array, usage);
                    };
                };
            };
        };
    };
};
exports.bufferData2 = function (_) {
    return function (ctx) {
        return function (target) {
            return function (size) {
                return function (usage) {
                    return function () {
                        ctx.bufferData(target, size, usage);
                    };
                };
            };
        };
    };
};
exports.bufferData3 = function (_) {
    return function (ctx) {
        return function (target) {
            return function (array) {
                return function (usage) {
                    return function (offset) {
                        return function (length) {
                            return function () {
                                ctx.bufferData(target, array, usage, offset, length);
                            };
                        };
                    };
                };
            };
        };
    };
};
exports.deleteBuffer = function (ctx) {
    return function (buffer) {
        return function () {
            ctx.deleteBuffer(buffer);
        };
    };
};
exports.bufferSubData = function (_) {
    return function (ctx) {
        return function (target) {
            return function (offset) {
                return function (array) {
                    return function () {
                        ctx.bufferSubData(target, offset, array);
                    };
                };
            };
        };
    };
};

// Shaders
exports.createShader = function (ctx) {
    return function (type) {
        return function () {
            return ctx.createShader(type);
        };
    };
};
exports.createProgram = function (ctx) {
    return function () {
        return ctx.createProgram();
    };
};
exports.shaderSource = function (ctx) {
    return function (shader) {
        return function (source) {
            return function () {
                ctx.shaderSource(shader, source);
            };
        };
    };
};
exports.compileShader = function (ctx) {
    return function (shader) {
        return function () {
            ctx.compileShader(shader);
        };
    };
};
exports.getShaderInfoLog = function (ctx) {
    return function (shader) {
        return function () {
            return ctx.getShaderInfoLog(shader);
        };
    };
};
exports.attachShader = function (ctx) {
    return function (program) {
        return function (shader) {
            return function () {
                ctx.attachShader(program, shader);
            };
        };
    };
};
exports.deleteProgram = function (ctx) {
    return function (program) {
        return function () {
            ctx.deleteProgram(program);
        };
    };
};
exports.deleteShader = function (ctx) {
    return function (shader) {
        return function () {
            ctx.deleteShader(shader);
        };
    };
};
exports.getProgramInfoLog = function (ctx) {
    return function (program) {
        return function () {
            return ctx.getProgramInfoLog(program);
        };
    };
};
exports.linkProgram = function (ctx) {
    return function (program) {
        return function () {
            ctx.linkProgram(program);
        };
    };
};
exports.useProgram = function (ctx) {
    return function (program) {
        return function () {
            ctx.useProgram(program);
        };
    };
};
exports.getShaderParameter = function (ctx) {
    return function (shader) {
        return function (pname) {
            return function () {
                return ctx.getShaderParameter(shader, pname);
            };
        };
    };
};
exports.getProgramParameter = function (ctx) {
    return function (program) {
        return function (pname) {
            return function () {
                return ctx.getProgramParameter(program, pname);
            };
        };
    };
};

// VAOs
exports.createVertexArray = function (ctx) {
    return function () {
        if (ctx.createVertexArray) return ctx.createVertexArray();

        var ext = ctx.getExtension('OES_vertex_array_object');

        if (ext) return ext.createVertexArrayOES();

        throw "OES_vertex_array_object extension not found.";
    };
};

exports.bindVertexArray = function (ctx) {
    return function (vao) {
        return function () {
            if (ctx.bindVertexArray) return ctx.bindVertexArray(vao);

            var ext = ctx.getExtension('OES_vertex_array_object');

            if (ext) return ext.bindVertexArrayOES(vao);

            throw "OES_vertex_array_object extension not found.";
        };
    };
};

exports.deleteVertexArray = function (ctx) {
    return function (vao) {
        return function () {
            if (ctx.deleteVertexArray) return ctx.deleteVertexArray(vao);

            var ext = ctx.getExtension('OES_vertex_array_object');

            if (ext) return ext.deleteVertexArrayOES(vao);

            throw "OES_vertex_array_object extension not found.";
        };
    };
};

// Vertex Attributes
exports.enableVertexAttribArray = function (ctx) {
    return function (i) {
        return function () {
            ctx.enableVertexAttribArray(i);
        };
    };
};
exports.disableVertexAttribArray = function (ctx) {
    return function (i) {
        return function () {
            ctx.disableVertexAttribArray(i);
        };
    };
};
exports.getAttribLocation = function (ctx) {
    return function (program) {
        return function (name) {
            return function () {
                return ctx.getAttribLocation(program, name);
            };
        };
    };
};
exports.getUniformLocation = function (ctx) {
    return function (program) {
        return function (name) {
            return function () {
                return ctx.getUniformLocation(program, name);
            };
        };
    };
};
exports.vertexAttribPointer = function (ctx) {
    return function (i) {
        return function (s) {
            return function (t) {
                return function (n) {
                    return function (st) {
                        return function (o) {
                            return function () {
                                ctx.vertexAttribPointer(i, s, t, n, st, o);
                            };
                        };
                    };
                };
            };
        };
    };
};
exports.vertexAttribIPointer = function (ctx) {
    return function (i) {
        return function (s) {
            return function (t) {
                return function (n) {
                    return function (st) {
                        return function (o) {
                            return function () {
                                ctx.vertexAttribIPointer(i, s, t, n, st, o);
                            };
                        };
                    };
                };
            };
        };
    };
};

// Uniforms
exports.uniform1f = function (ctx) {
    return function (loc) {
        return function (v) {
            return function () {
                ctx.uniform1f(loc, v);
            };
        };
    };
};
exports.uniform1i = function (ctx) {
    return function (loc) {
        return function (v) {
            return function () {
                ctx.uniform1i(loc, v);
            };
        };
    };
};
exports.uniform1fv = function (ctx) {
    return function (loc) {
        return function (v) {
            return function () {
                ctx.uniform1fv(loc, v);
            };
        };
    };
};
exports.uniform1iv = function (ctx) {
    return function (loc) {
        return function (v) {
            return function () {
                ctx.uniform1iv(loc, v);
            };
        };
    };
};

exports.uniform2f = function (ctx) {
    return function (loc) {
        return function (v1) {
            return function (v2) {
                return function () {
                    ctx.uniform2f(loc, v1, v2);
                };
            };
        };
    };
};
exports.uniform2i = function (ctx) {
    return function (loc) {
        return function (v1) {
            return function (v2) {
                return function () {
                    ctx.uniform2i(loc, v1, v2);
                };
            };
        };
    };
};
exports.uniform2fv = function (ctx) {
    return function (loc) {
        return function (v) {
            return function () {
                ctx.uniform2fv(loc, v);
            };
        };
    };
};
exports.uniform2iv = function (ctx) {
    return function (loc) {
        return function (v) {
            return function () {
                ctx.uniform2iv(loc, v);
            };
        };
    };
};

exports.uniform3f = function (ctx) {
    return function (loc) {
        return function (v1) {
            return function (v2) {
                return function (v3) {
                    return function () {
                        ctx.uniform3f(loc, v1, v2, v3);
                    };
                };
            };
        };
    };
};
exports.uniform3i = function (ctx) {
    return function (loc) {
        return function (v1) {
            return function (v2) {
                return function (v3) {
                    return function () {
                        ctx.uniform3i(loc, v1, v2, v3);
                    };
                };
            };
        };
    };
};
exports.uniform3fv = function (ctx) {
    return function (loc) {
        return function (v) {
            return function () {
                ctx.uniform3fv(loc, v);
            };
        };
    };
};
exports.uniform3iv = function (ctx) {
    return function (loc) {
        return function (v) {
            return function () {
                ctx.uniform3iv(loc, v);
            };
        };
    };
};

exports.uniform4f = function (ctx) {
    return function (loc) {
        return function (v1) {
            return function (v2) {
                return function (v3) {
                    return function (v4) {
                        return function () {
                            ctx.uniform4f(loc, v1, v2, v3, v4);
                        };
                    };
                };
            };
        };
    };
};
exports.uniform4i = function (ctx) {
    return function (loc) {
        return function (v1) {
            return function (v2) {
                return function (v3) {
                    return function (v4) {
                        return function () {
                            ctx.uniform4i(loc, v1, v2, v3, v4);
                        };
                    };
                };
            };
        };
    };
};
exports.uniform4fv = function (ctx) {
    return function (loc) {
        return function (v) {
            return function () {
                ctx.uniform4fv(loc, v);
            };
        };
    };
};
exports.uniform4iv = function (ctx) {
    return function (loc) {
        return function (v) {
            return function () {
                ctx.uniform4iv(loc, v);
            };
        };
    };
};

exports.uniformMatrix2fv = function (ctx) {
    return function (loc) {
        return function (t) {
            return function (v) {
                return function () {
                    ctx.uniformMatrix2fv(loc, t, v);
                };
            };
        };
    };
};
exports.uniformMatrix3fv = function (ctx) {
    return function (loc) {
        return function (t) {
            return function (v) {
                return function () {
                    ctx.uniformMatrix3fv(loc, t, v);
                };
            };
        };
    };
};
exports.uniformMatrix4fv = function (ctx) {
    return function (loc) {
        return function (t) {
            return function (v) {
                return function () {
                    ctx.uniformMatrix4fv(loc, t, v);
                };
            };
        };
    };
};

exports.uniform1ui = function (ctx) {
    return function (loc) {
        return function (v) {
            return function () {
                ctx.uniform1ui(loc, v);
            };
        };
    };
};
exports.uniform2ui = function (ctx) {
    return function (loc) {
        return function (v1) {
            return function (v2) {
                return function () {
                    ctx.uniform1ui(loc, v1, v2);
                };
            };
        };
    };
};
exports.uniform3ui = function (ctx) {
    return function (loc) {
        return function (v1) {
            return function (v2) {
                return function (v3) {
                    return function () {
                        ctx.uniform1ui(loc, v1, v2, v3);
                    };
                };
            };
        };
    };
};
exports.uniform4ui = function (ctx) {
    return function (loc) {
        return function (v1) {
            return function (v2) {
                return function (v3) {
                    return function (v4) {
                        return function () {
                            ctx.uniform1ui(loc, v1, v2, v3, v4);
                        };
                    };
                };
            };
        };
    };
};

exports.uniform1uiv = function (ctx) {
    return function (loc) {
        return function (v) {
            return function () {
                ctx.uniform1uiv(loc, v);
            };
        };
    };
};
exports.uniform2uiv = function (ctx) {
    return function (loc) {
        return function (v) {
            return function () {
                ctx.uniform1uiv(loc, v);
            };
        };
    };
};
exports.uniform3uiv = function (ctx) {
    return function (loc) {
        return function (v) {
            return function () {
                ctx.uniform1uiv(loc, v);
            };
        };
    };
};
exports.uniform4uiv = function (ctx) {
    return function (loc) {
        return function (v) {
            return function () {
                ctx.uniform1uiv(loc, v);
            };
        };
    };
};

exports.drawArrays = function (ctx) {
    return function (mode) {
        return function (first) {
            return function (count) {
                return function () {
                    ctx.drawArrays(mode, first, count);
                };
            };
        };
    };
};
exports.drawElements = function (ctx) {
    return function (mode) {
        return function (count) {
            return function (type) {
                return function (offset) {
                    return function () {
                        ctx.drawElements(mode, first, count);
                    };
                };
            };
        };
    };
};

exports.getShaderParameter = function (ctx) {
    return function (shader) {
        return function (pname) {
            return function () {
                return ctx.getShaderParameter(shader, pname);
            };
        };
    };
};
exports.getProgramParameter = function (ctx) {
    return function (program) {
        return function (pname) {
            return function () {
                return ctx.getProgramParameter(program, pname);
            };
        };
    };
};

exports.clear = function (ctx) {
    return function (b) {
        return function () {
            ctx.clear(b);
        };
    };
};
exports.clearColor = function (ctx) {
    return function (r) {
        return function (g) {
            return function (b) {
                return function (a) {
                    return function () {
                        ctx.clearColor(r, g, b, a);
                    };
                };
            };
        };
    };
};

exports.getExtension = function (ctx) {
    return function (name) {
        return function () {
            return ctx.getExtension(name);
        };
    };
};

exports.createTexture = function (ctx) {
    return function () {
        return ctx.createTexture();
    };
};
exports.bindTexture = function (ctx) {
    return function (t) {
        return function (tex) {
            return function () {
                ctx.bindTexture(t, tex);
            };
        };
    };
};
exports.texParameteri = function (ctx) {
    return function (t) {
        return function (pname) {
            return function (val) {
                return function () {
                    ctx.texParameteri(t, pname, val);
                };
            };
        };
    };
};
exports.texParameterf = function (ctx) {
    return function (t) {
        return function (pname) {
            return function (val) {
                return function () {
                    ctx.texParameterf(t, pname, val);
                };
            };
        };
    };
};
exports.generateMipmap = function (ctx) {
    return function (t) {
        return function () {
            ctx.generateMipmap(t);
        };
    };
};
exports.activeTexture = function (ctx) {
    return function (slot) {
        return function () {
            ctx.activeTexture(slot);
        };
    };
};
exports.texImage2D = function (_) {
    return function (ctx) {
        return function (target) {
            return function (level) {
                return function (iformat) {
                    return function (format) {
                        return function (type) {
                            return function (pixels) {
                                return function () {
                                    ctx.texImage2D(target, level, iformat, format, type, pixels);
                                };
                            };
                        };
                    };
                };
            };
        };
    };
};
exports.texImage2D2 = function (_) {
    return function (ctx) {
        return function (target) {
            return function (level) {
                return function (iformat) {
                    return function (width) {
                        return function (height) {
                            return function (format) {
                                return function (type) {
                                    return function () {
                                        ctx.texImage2D(target, level, iformat, width, height, 0, format, type, null);
                                    };
                                };
                            };
                        };
                    };
                };
            };
        };
    };
};

exports.createFramebuffer = function (ctx) {
    return function () {
        return ctx.createFramebuffer();
    };
};
exports.bindFramebuffer = function (ctx) {
    return function (target) {
        return function (fbo) {
            return function () {
                ctx.bindFramebuffer(target, fbo);
            };
        };
    };
};
exports.deleteFramebuffer = function (ctx) {
    return function (fbo) {
        return function () {
            ctx.deleteFramebuffer(fbo);
        };
    };
};
exports.framebufferTexture2D = function (ctx) {
    return function (target) {
        return function (att) {
            return function (textarget) {
                return function (texture) {
                    return function (level) {
                        return function () {
                            ctx.framebufferTexture2D(target, att, textarget, texture, level);
                        };
                    };
                };
            };
        };
    };
};
exports.framebufferRenderbuffer = function (ctx) {
    return function (target) {
        return function (att) {
            return function (rbtarget) {
                return function (rb) {
                    return function () {
                        ctx.framebufferRenderbuffer(target, att, rbtarget, rb);
                    };
                };
            };
        };
    };
};

exports.createRenderbuffer = function (ctx) {
    return function () {
        return ctx.createRenderbuffer();
    };
};
exports.bindRenderbuffer = function (ctx) {
    return function (target) {
        return function (rb) {
            return function () {
                ctx.bindRenderbuffer(target, rb);
            };
        };
    };
};
exports.deleteRenderbuffer = function (ctx) {
    return function (rb) {
        return function () {
            ctx.deleteRenderbuffer(rb);
        };
    };
};
exports.renderbufferStorage = function (ctx) {
    return function (target) {
        return function (format) {
            return function (w) {
                return function (h) {
                    return function () {
                        ctx.renderbufferStorage(target, format, w, h);
                    };
                };
            };
        };
    };
};

/*-------  VAO Polyfill from https://github.com/greggman/oes-vertex-array-object-polyfill -------*/

/*
** Copyright (c) 2015 The Khronos Group Inc.
**
** Permission is hereby granted, free of charge, to any person obtaining a
** copy of this software and/or associated documentation files (the
** "Materials"), to deal in the Materials without restriction, including
** without limitation the rights to use, copy, modify, merge, publish,
** distribute, sublicense, and/or sell copies of the Materials, and to
** permit persons to whom the Materials are furnished to do so, subject to
** the following conditions:
**
** The above copyright notice and this permission notice shall be included
** in all copies or substantial portions of the Materials.
**
** THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
** MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
*/
(function () {
    "use strict";

    var glErrorShadow = {};

    function error(msg) {
        if (window.console && window.console.error) {
            window.console.error(msg);
        }
    }

    function log(msg) {
        if (window.console && window.console.log) {
            window.console.log(msg);
        }
    }

    function synthesizeGLError(err, opt_msg) {
        glErrorShadow[err] = true;
        if (opt_msg !== undefined) {
            error(opt_msg);
        }
    }

    function wrapGLError(gl) {
        var f = gl.getError;
        gl.getError = function () {
            var err;
            do {
                err = f.apply(gl);
                if (err != gl.NO_ERROR) {
                    glErrorShadow[err] = true;
                }
            } while (err != gl.NO_ERROR);
            for (var err in glErrorShadow) {
                if (glErrorShadow[err]) {
                    delete glErrorShadow[err];
                    return parseInt(err);
                }
            }
            return gl.NO_ERROR;
        };
    }

    var WebGLVertexArrayObjectOES = function WebGLVertexArrayObjectOES(ext) {
        var gl = ext.gl;

        this.ext = ext;
        this.isAlive = true;
        this.hasBeenBound = false;

        this.elementArrayBuffer = null;
        this.attribs = new Array(ext.maxVertexAttribs);
        for (var n = 0; n < this.attribs.length; n++) {
            var attrib = new WebGLVertexArrayObjectOES.VertexAttrib(gl);
            this.attribs[n] = attrib;
        }

        this.maxAttrib = 0;
    };

    WebGLVertexArrayObjectOES.VertexAttrib = function VertexAttrib(gl) {
        this.enabled = false;
        this.buffer = null;
        this.size = 4;
        this.type = gl.FLOAT;
        this.normalized = false;
        this.stride = 16;
        this.offset = 0;

        this.cached = "";
        this.recache();
    };
    WebGLVertexArrayObjectOES.VertexAttrib.prototype.recache = function recache() {
        this.cached = [this.size, this.type, this.normalized, this.stride, this.offset].join(":");
    };

    var OESVertexArrayObject = function OESVertexArrayObject(gl) {
        var self = this;
        this.gl = gl;

        wrapGLError(gl);

        var original = this.original = {
            getParameter: gl.getParameter,
            enableVertexAttribArray: gl.enableVertexAttribArray,
            disableVertexAttribArray: gl.disableVertexAttribArray,
            bindBuffer: gl.bindBuffer,
            getVertexAttrib: gl.getVertexAttrib,
            vertexAttribPointer: gl.vertexAttribPointer
        };

        gl.getParameter = function getParameter(pname) {
            if (pname == self.VERTEX_ARRAY_BINDING_OES) {
                if (self.currentVertexArrayObject == self.defaultVertexArrayObject) {
                    return null;
                } else {
                    return self.currentVertexArrayObject;
                }
            }
            return original.getParameter.apply(this, arguments);
        };

        gl.enableVertexAttribArray = function enableVertexAttribArray(index) {
            var vao = self.currentVertexArrayObject;
            vao.maxAttrib = Math.max(vao.maxAttrib, index);
            var attrib = vao.attribs[index];
            attrib.enabled = true;
            return original.enableVertexAttribArray.apply(this, arguments);
        };
        gl.disableVertexAttribArray = function disableVertexAttribArray(index) {
            var vao = self.currentVertexArrayObject;
            vao.maxAttrib = Math.max(vao.maxAttrib, index);
            var attrib = vao.attribs[index];
            attrib.enabled = false;
            return original.disableVertexAttribArray.apply(this, arguments);
        };

        gl.bindBuffer = function bindBuffer(target, buffer) {
            switch (target) {
                case gl.ARRAY_BUFFER:
                    self.currentArrayBuffer = buffer;
                    break;
                case gl.ELEMENT_ARRAY_BUFFER:
                    self.currentVertexArrayObject.elementArrayBuffer = buffer;
                    break;
            }
            return original.bindBuffer.apply(this, arguments);
        };

        gl.getVertexAttrib = function getVertexAttrib(index, pname) {
            var vao = self.currentVertexArrayObject;
            var attrib = vao.attribs[index];
            switch (pname) {
                case gl.VERTEX_ATTRIB_ARRAY_BUFFER_BINDING:
                    return attrib.buffer;
                case gl.VERTEX_ATTRIB_ARRAY_ENABLED:
                    return attrib.enabled;
                case gl.VERTEX_ATTRIB_ARRAY_SIZE:
                    return attrib.size;
                case gl.VERTEX_ATTRIB_ARRAY_STRIDE:
                    return attrib.stride;
                case gl.VERTEX_ATTRIB_ARRAY_TYPE:
                    return attrib.type;
                case gl.VERTEX_ATTRIB_ARRAY_NORMALIZED:
                    return attrib.normalized;
                default:
                    return original.getVertexAttrib.apply(this, arguments);
            }
        };

        gl.vertexAttribPointer = function vertexAttribPointer(indx, size, type, normalized, stride, offset) {
            var vao = self.currentVertexArrayObject;
            vao.maxAttrib = Math.max(vao.maxAttrib, indx);
            var attrib = vao.attribs[indx];
            attrib.buffer = self.currentArrayBuffer;
            attrib.size = size;
            attrib.type = type;
            attrib.normalized = normalized;
            attrib.stride = stride;
            attrib.offset = offset;
            attrib.recache();
            return original.vertexAttribPointer.apply(this, arguments);
        };

        if (gl.instrumentExtension) {
            gl.instrumentExtension(this, "OES_vertex_array_object");
        }

        gl.canvas.addEventListener('webglcontextrestored', function () {
            log("OESVertexArrayObject emulation library context restored");
            self.reset_();
        }, true);

        this.reset_();
    };

    OESVertexArrayObject.prototype.VERTEX_ARRAY_BINDING_OES = 0x85B5;

    OESVertexArrayObject.prototype.reset_ = function reset_() {
        var contextWasLost = this.vertexArrayObjects !== undefined;
        if (contextWasLost) {
            for (var ii = 0; ii < this.vertexArrayObjects.length; ++ii) {
                this.vertexArrayObjects.isAlive = false;
            }
        }
        var gl = this.gl;
        this.maxVertexAttribs = gl.getParameter(gl.MAX_VERTEX_ATTRIBS);

        this.defaultVertexArrayObject = new WebGLVertexArrayObjectOES(this);
        this.currentVertexArrayObject = null;
        this.currentArrayBuffer = null;
        this.vertexArrayObjects = [this.defaultVertexArrayObject];

        this.bindVertexArrayOES(null);
    };

    OESVertexArrayObject.prototype.createVertexArrayOES = function createVertexArrayOES() {
        var arrayObject = new WebGLVertexArrayObjectOES(this);
        this.vertexArrayObjects.push(arrayObject);
        return arrayObject;
    };

    OESVertexArrayObject.prototype.deleteVertexArrayOES = function deleteVertexArrayOES(arrayObject) {
        arrayObject.isAlive = false;
        this.vertexArrayObjects.splice(this.vertexArrayObjects.indexOf(arrayObject), 1);
        if (this.currentVertexArrayObject == arrayObject) {
            this.bindVertexArrayOES(null);
        }
    };

    OESVertexArrayObject.prototype.isVertexArrayOES = function isVertexArrayOES(arrayObject) {
        if (arrayObject && arrayObject instanceof WebGLVertexArrayObjectOES) {
            if (arrayObject.hasBeenBound && arrayObject.ext == this) {
                return true;
            }
        }
        return false;
    };

    OESVertexArrayObject.prototype.bindVertexArrayOES = function bindVertexArrayOES(arrayObject) {
        var gl = this.gl;
        if (arrayObject && !arrayObject.isAlive) {
            synthesizeGLError(gl.INVALID_OPERATION, "bindVertexArrayOES: attempt to bind deleted arrayObject");
            return;
        }
        var original = this.original;

        var oldVAO = this.currentVertexArrayObject;
        this.currentVertexArrayObject = arrayObject || this.defaultVertexArrayObject;
        this.currentVertexArrayObject.hasBeenBound = true;
        var newVAO = this.currentVertexArrayObject;

        if (oldVAO == newVAO) {
            return;
        }

        if (!oldVAO || newVAO.elementArrayBuffer != oldVAO.elementArrayBuffer) {
            original.bindBuffer.call(gl, gl.ELEMENT_ARRAY_BUFFER, newVAO.elementArrayBuffer);
        }

        var currentBinding = this.currentArrayBuffer;
        var maxAttrib = Math.max(oldVAO ? oldVAO.maxAttrib : 0, newVAO.maxAttrib);
        for (var n = 0; n <= maxAttrib; n++) {
            var attrib = newVAO.attribs[n];
            var oldAttrib = oldVAO ? oldVAO.attribs[n] : null;

            if (!oldVAO || attrib.enabled != oldAttrib.enabled) {
                if (attrib.enabled) {
                    original.enableVertexAttribArray.call(gl, n);
                } else {
                    original.disableVertexAttribArray.call(gl, n);
                }
            }

            if (attrib.enabled) {
                var bufferChanged = false;
                if (!oldVAO || attrib.buffer != oldAttrib.buffer) {
                    if (currentBinding != attrib.buffer) {
                        original.bindBuffer.call(gl, gl.ARRAY_BUFFER, attrib.buffer);
                        currentBinding = attrib.buffer;
                    }
                    bufferChanged = true;
                }

                if (bufferChanged || attrib.cached != oldAttrib.cached) {
                    original.vertexAttribPointer.call(gl, n, attrib.size, attrib.type, attrib.normalized, attrib.stride, attrib.offset);
                }
            }
        }

        if (this.currentArrayBuffer != currentBinding) {
            original.bindBuffer.call(gl, gl.ARRAY_BUFFER, this.currentArrayBuffer);
        }
    };

    function setupVertexArrayObject() {
        var original_getSupportedExtensions = WebGLRenderingContext.prototype.getSupportedExtensions;
        WebGLRenderingContext.prototype.getSupportedExtensions = function getSupportedExtensions() {
            var list = original_getSupportedExtensions.call(this) || [];
            if (list.indexOf("OES_vertex_array_object") < 0) {
                list.push("OES_vertex_array_object");
            }
            return list;
        };

        var original_getExtension = WebGLRenderingContext.prototype.getExtension;
        WebGLRenderingContext.prototype.getExtension = function getExtension(name) {
            var ext = original_getExtension.call(this, name);
            if (ext) {
                return ext;
            }
            if (name !== "OES_vertex_array_object") {
                return null;
            }

            if (!this.__OESVertexArrayObject) {
                console.log("Setup OES_vertex_array_object polyfill");
                this.__OESVertexArrayObject = new OESVertexArrayObject(this);
            }
            return this.__OESVertexArrayObject;
        };
    }

    setupVertexArrayObject();
})();