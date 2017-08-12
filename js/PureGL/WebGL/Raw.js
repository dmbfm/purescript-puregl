exports.glNullBufferObject = null;
exports.glNullVertexArrayObject = null;
exports.glNullFramebufferObject = null;

// Buffers
exports.glCreateBuffer = ctx => () => ctx.createBuffer();
exports.glBindBuffer = ctx => target => buffer => () => { ctx.bindBuffer(target, buffer); };
exports.glBufferData = _ => ctx => target => array => usage => () => { ctx.bufferData(target, array, usage); };
exports.glBufferData2 = _ => ctx => target => size => usage => () => { ctx.bufferData(target, size, usage); };
exports.glBufferData3 = _ => ctx => target => array => 
      usage => offset => length => () => { ctx.bufferData(target, array, usage, offset, length); };
exports.glDeleteBuffer = ctx => buffer => () => { ctx.deleteBuffer(buffer); };
exports.glBufferSubData = _ => ctx => target => offset => array => () => { ctx.bufferSubData(target, offset, array); };


// Shaders
exports.glCreateShader = ctx => type => () => ctx.createShader(type);
exports.glCreateProgram = ctx => () => ctx.createProgram();
exports.glShaderSource = ctx => shader => source => () => { ctx.shaderSource(shader, source); };
exports.glCompileShader = ctx => shader => () => { ctx.compileShader(shader); };
exports.glGetShaderInfoLog = ctx => shader => () => ctx.getShaderInfoLog(shader);
exports.glAttachShader = ctx => program => shader => () => { ctx.attachShader(program, shader); };
exports.glDeleteProgram = ctx => program => () => { ctx.deleteProgram(program); };
exports.glDeleteShader = ctx => shader => () => { ctx.deleteShader(shader); };
exports.glGetProgramInfoLog = ctx => program => () => ctx.getProgramInfoLog(program);
exports.glLinkProgram = ctx => program => () => { ctx.linkProgram(program); };
exports.glUseProgram = ctx => program => () => { ctx.useProgram(program); };
exports.glGetShaderParameter = ctx => shader => pname => () => ctx.getShaderParameter(shader, pname);
exports.glGetProgramParameter = ctx => program => pname => () => ctx.getProgramParameter(program, pname);

// VAOs
exports.glCreateVertexArray = ctx => () => {
  if (ctx.createVertexArray)
    return ctx.createVertexArray();

  let ext = ctx.getExtension('OES_vertex_array_object');

  if (ext)
    return ext.createVertexArrayOES();

  throw "OES_vertex_array_object extension not found.";

};

exports.glBindVertexArray = ctx => vao => () => {
  if (ctx.bindVertexArray)
    return ctx.bindVertexArray(vao);

  let ext = ctx.getExtension('OES_vertex_array_object');

  if (ext)
    return ext.bindVertexArrayOES(vao);

  throw "OES_vertex_array_object extension not found.";
}

exports.glDeleteVertexArray = ctx => vao => () => {
  if (ctx.deleteVertexArray)
    return ctx.deleteVertexArray(vao);

  let ext = ctx.getExtension('OES_vertex_array_object');

  if (ext)
    return ext.deleteVertexArrayOES(vao);

  throw "OES_vertex_array_object extension not found.";
};


// Vertex Attributes
exports.glEnableVertexAttribArray = ctx => i => () => { ctx.enableVertexAttribArray(i); };
exports.glDisableVertexAttribArray = ctx => i => () => { ctx.disableVertexAttribArray(i); };
exports.glGetAttribLocation = ctx => program => name => () => ctx.getAttribLocation(program, name);
exports.glGetUniformLocation = ctx => program => name => () => ctx.getUniformLocation(program, name);
exports.glVertexAttribPointer = ctx => i => s => t => n => st => o => () =>
  { ctx.vertexAttribPointer(i, s, t, n, st, o); };
exports.glVertexAttribIPointer = ctx => i => s => t => n => st => o => () =>
  { ctx.vertexAttribIPointer(i, s, t, n, st, o); };

// Uniforms
exports.glUniform1f = ctx => loc => v => () => { ctx.uniform1f(loc, v); };
exports.glUniform1i = ctx => loc => v => () => { ctx.uniform1i(loc, v); };
exports.glUniform1fv = ctx => loc => v => () => { ctx.uniform1fv(loc, v); };
exports.glUniform1iv = ctx => loc => v => () => { ctx.uniform1iv(loc, v); };

exports.glUniform2f = ctx => loc => v1 => v2 => () => { ctx.uniform2f(loc, v1, v2); };
exports.glUniform2i = ctx => loc => v1 => v2 => () => { ctx.uniform2i(loc, v1, v2); };
exports.glUniform2fv =  ctx => loc => v => () => { ctx.uniform2fv(loc, v); };
exports.glUniform2iv =  ctx => loc => v => () => { ctx.uniform2iv(loc, v); };

exports.glUniform3f = ctx => loc => v1 => v2 => v3 => () => { ctx.uniform3f(loc, v1, v2, v3); };
exports.glUniform3i = ctx => loc => v1 => v2 => v3 => () => { ctx.uniform3i(loc, v1, v2, v3); };
exports.glUniform3fv = ctx => loc => v => () => { ctx.uniform3fv(loc, v); };
exports.glUniform3iv = ctx => loc => v => () => { ctx.uniform3iv(loc, v); };

exports.glUniform4f = ctx => loc => v1 => v2 => v3 => v4 => () => { ctx.uniform4f(loc, v1, v2, v3, v4); };
exports.glUniform4i = ctx => loc => v1 => v2 => v3 => v4 => () => { ctx.uniform4i(loc, v1, v2, v3, v4); };
exports.glUniform4fv = ctx => loc => v => () => { ctx.uniform4fv(loc, v); };
exports.glUniform4iv = ctx => loc => v => () => { ctx.uniform4iv(loc, v); };

exports.glUniformMatrix2fv = ctx => loc => t => v => () => { ctx.uniformMatrix2fv(loc, t, v); };
exports.glUniformMatrix3fv = ctx => loc => t => v => () => { ctx.uniformMatrix3fv(loc, t, v); };
exports.glUniformMatrix4fv = ctx => loc => t => v => () => { ctx.uniformMatrix4fv(loc, t, v); };

exports.glUniform1ui = ctx => loc => v => () => { ctx.uniform1ui(loc, v); };
exports.glUniform2ui = ctx => loc => v1 => v2 => () => { ctx.uniform1ui(loc, v1, v2); };
exports.glUniform3ui = ctx => loc => v1 => v2 => v3 => () => { ctx.uniform1ui(loc, v1, v2, v3); };
exports.glUniform4ui = ctx => loc => v1 => v2 => v3 => v4 => () => { ctx.uniform1ui(loc, v1, v2, v3, v4); };

exports.glUniform1uiv = ctx => loc => v => () => { ctx.uniform1uiv(loc, v); };
exports.glUniform2uiv = ctx => loc => v => () => { ctx.uniform1uiv(loc, v); };
exports.glUniform3uiv = ctx => loc => v => () => { ctx.uniform1uiv(loc, v); };
exports.glUniform4uiv = ctx => loc => v => () => { ctx.uniform1uiv(loc, v); };

exports.glDrawArrays = ctx => mode => first => count => () => { ctx.drawArrays(mode, first, count); };
exports.glDrawElements = ctx => mode => count => type => offset => () => { ctx.drawElements(mode, first, count); };

exports.glGetShaderParameter = ctx => shader => pname => () => ctx.getShaderParameter(shader, pname);
exports.glGetProgramParameter = ctx => program => pname => () => ctx.getProgramParameter(program, pname);

exports.glClear = ctx => b => () => { ctx.clear(b); };
exports.glClearColor = ctx => r => g => b => a => () => { ctx.clearColor(r, g, b, a); };

exports.glGetExtension = ctx => name => () => ctx.getExtension(name);

exports.glCreateTexture = ctx => () => ctx.createTexture();
exports.glBindTexture = ctx => t => tex => () => { ctx.bindTexture(t, tex); };
exports.glTexParameteri = ctx => t => pname => val => () => { ctx.texParameteri(t, pname, val); };
exports.glTexParameterf = ctx => t => pname => val => () => { ctx.texParameterf(t, pname, val); };
exports.glGenerateMipmap = ctx => t => () => { ctx.generateMipmap(t); };
exports.glActiveTexture = ctx => slot => () => { ctx.activeTexture(slot); };
exports.glTexImage2D = _ => ctx => target => level => iformat => format => type => pixels => () =>
{ ctx.texImage2D(target, level, iformat, format, type, pixels); };
exports.glTexImage2D2 = _ => ctx => target => level => iformat => width => height => format => type => () =>
{ ctx.texImage2D(target, level, iformat, width, height, 0, format, type, null); };    


exports.glCreateFramebuffer = ctx => () => ctx.createFramebuffer();
exports.glBindFramebuffer = ctx => target => fbo => () => { ctx.bindFramebuffer(target, fbo); };
exports.glDeleteFramebuffer = ctx => fbo => () => { ctx.deleteFramebuffer(fbo); };
exports.glFramebufferTexture2D = ctx => target => att => textarget => texture => level => () =>
    { ctx.framebufferTexture2D(target, att, textarget, texture, level); };
exports.glFramebufferRenderbuffer = ctx => target => att => rbtarget => rb => () =>
    { ctx.framebufferRenderbuffer(target, att, rbtarget, rb); };    

exports.glCreateRenderbuffer = ctx => () => ctx.createRenderbuffer();
exports.glBindRenderbuffer = ctx => target => rb => () => { ctx.bindRenderbuffer(target, rb); };
exports.glDeleteRenderbuffer = ctx => rb => () => { ctx.deleteRenderbuffer(rb); };
exports.glRenderbufferStorage = ctx => target => format => w => h => () => { ctx.renderbufferStorage(target, format, w, h); };

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
(function() {
"use strict";

var glErrorShadow = { };

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
        error(opt_msg)
    }
}

function wrapGLError(gl) {
    var f = gl.getError;
    gl.getError = function() {
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

    gl.canvas.addEventListener('webglcontextrestored', function() {
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
        var ext =  original_getExtension.call(this, name);
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

}());