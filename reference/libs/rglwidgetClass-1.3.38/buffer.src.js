/**
     * Methods related to buffered data
     * @name ___METHODS_FOR_BUFFERS___
     * @memberof rglwidgetClass
     * @kind function
     * @instance
     */
    /**
     * Detect rglBuffered object
     * @param { Object } obj - vertices or similar 
     */
    rglwidgetClass.prototype.isBuffered = function(obj) {
      return typeof obj === "string";
    };

    /* The next two functions are taken from 
     
     https://developer.mozilla.org/en-US/docs/Web/JavaScript/Base64_encoding_and_decoding
     
     They were written by Mozilla Contributors and dedicated
     to the public domain under CC0. */
     
    /* Array of bytes to Base64 string decoding */
    rglwidgetClass.prototype.b64ToUint6 = function(nChr) {
      return nChr > 64 && nChr < 91 ? nChr - 65 : 
             nChr > 96 && nChr < 123 ? nChr - 71 : 
             nChr > 47 && nChr < 58 ? nChr + 4 : 
             nChr === 43 ? 62 : 
             nChr === 47 ? 63 : 
             0;
    };

    /* jshint bitwise:false */
    rglwidgetClass.prototype.base64DecToArr = function(sBase64, nBlocksSize) {
      var sB64Enc = sBase64.replace(/[^A-Za-z0-9\+\/]/g, ""),
        nInLen = sB64Enc.length, 
        nOutLen = nBlocksSize ? Math.ceil((nInLen * 3 + 1 >> 2) / nBlocksSize) * nBlocksSize : nInLen * 3 + 1 >> 2, 
        taBytes = new Uint8Array(nOutLen);
      for (var nMod3, nMod4, nUint24 = 0, nOutIdx = 0, nInIdx = 0; nInIdx < nInLen; nInIdx++) {
        nMod4 = nInIdx & 3;
        nUint24 |= this.b64ToUint6(sB64Enc.charCodeAt(nInIdx)) << 6 * (3 - nMod4);
        if (nMod4 === 3 || nInLen - nInIdx === 1) {
          for (nMod3 = 0; nMod3 < 3 && nOutIdx < nOutLen; nMod3++, nOutIdx++) {
            taBytes[nOutIdx] = nUint24 >>> (16 >>> nMod3 & 24) & 255;
          }
          nUint24 = 0;
        }
      }
      return taBytes;
    };
    /* jshint bitwise:true */
    
    rglwidgetClass.prototype.getArrayBuffer = function(base64) {
      return this.base64DecToArr(base64, 4).buffer;
    };

    rglwidgetClass.prototype.getBufferedData = function(v) {
      return this.readAccessor(parseInt(v, 10), this.scene.buffer);
    };
    
    rglwidgetClass.prototype.readAccessor = function(acc, buf) {
      var typeSignedByte = 5120, 
          typeUnsignedByte = 5121, 
          typeSignedShort = 5122, 
          typeUnsignedShort = 5123, 
          typeSignedInt = 5124, 
          typeUnsignedInt = 5125, 
          typeFloat = 5126, 
          typeDouble = 5130, 
          accessor = buf.accessors[acc], 
          bufferView = buf.bufferViews[accessor.bufferView], 
          buffer = buf.buffers[bufferView.buffer], 
          bytes, 
          lens = {
            SCALAR: 1,
            VEC2: 2,
            VEC3: 3,
            VEC4: 4,
            MAT2: 4,
            MAT3: 9,
            MAT4: 16
          }, 
          rowsizes = {
            SCALAR: 1,
            VEC2: 2,
            VEC3: 3,
            VEC4: 4,
            MAT2: 2,
            MAT3: 3,
            MAT4: 4
          }, 
          offset = 0, 
          len = lens[accessor.type], 
          rowsize = rowsizes[accessor.type], 
          count = len * accessor.count, 
          nrows = count / rowsize, 
          values, arr = [], row, i, j, k;
          
      if (typeof buffer.bytes === "string") 
        buffer.bytes = this.getArrayBuffer(buffer.bytes);
        
      bytes = buffer.bytes;
      
      if (typeof accessor.byteOffset !== "undefined") 
        offset += accessor.byteOffset;
        
      if (typeof bufferView.byteOffset !== "undefined") 
        offset += bufferView.byteOffset;
        
      switch (accessor.componentType) {
       case typeSignedByte:
        values = new Int8Array(buffer.bytes, offset, count);
        break;

       case typeUnsignedByte:
        values = new Uint8Array(buffer.bytes, offset, count);
        break;

       case typeSignedShort:
        values = new Int16Array(buffer.bytes, offset, count);
        break;

       case typeUnsignedShort:
        values = new Uint16Array(buffer.bytes, offset, count);
        break;

       case typeSignedInt:
        values = new Int32Array(buffer.bytes, offset, count);
        break;

       case typeUnsignedInt:
        values = new Uint32Array(buffer.bytes, offset, count);
        break;

       case typeFloat:
        values = new Float32Array(buffer.bytes, offset, count);
        break;

       case typeDouble:
        values = new Float64Array(buffer.bytes, offset, count);
        break;
      }

      /* This is all very inefficient, but is convenient
             to work with the old code. */
      k = 0;
      for (i = 0; i < nrows; i++) {
        row = [];
        for (j = 0; j < rowsize; j++) {
          if (accessor.normalized) {
            switch(accessor.componentType) {
              case typeSignedByte:
                row.push(Math.max(values[k++]/127, -1.0));
                break;
              case typeSignedShort:
                row.push(Math.max(values[k++]/32767, -1.0));
                break;
              case typeUnsignedByte:
                row.push(values[k++]/255);
                break;
              case typeUnsignedShort:
                row.push(values[k++]/65535);
                break;
            }
          } else
            row.push(values[k++]);
        }
        arr.push(row);
      }
      return arr;
    };
    
    rglwidgetClass.prototype.expandBufferedFields = function(obj) {
      /* this list needs to match the one in convertScene.R */
      var fields = ["vertices", "normals", "indices", 
                    "texcoords", "colors", "centers"], i, field;
      for (i = 0; i < fields.length; i++) {
        field = obj[fields[i]];
        if (this.isBuffered(field))
          obj[fields[i]] = this.getBufferedData(field);
      }
    };
