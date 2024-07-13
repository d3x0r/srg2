'use strict';

const _assert = false;

// usage
//  var RNG = require( "salty_random_generator")( callback }
//    constructor callback is used as a source of salt to the generator
//    the callback is passed an array to which strings are expected to be added
//     ( [] )=>{ [].push( more_salt ); }
//
//    - methods on RNG
//         reset()
//                clear current random state, and restart
//
//         getBits( /* 0-31 */ )
//                return a Number that is that many bits from the random stream
//
//         getBuffer( /* 0-n */ )
//                returns a ArrayBuffer that is that many bits of randomness...
//
//         save()
//                return an object representing the current RNG state
//
//         restore( o )
//                use object to restore RNG state.
//
//          feed( buf )
//                feed a raw uint8array.
//


//var exports = exports || {};

	// My JS Encoding $_ and = at the end.  allows most to be identifiers too.
	// 'standard' encoding + /
	// variants            - /
	//                     + ,
	//                     . _
	// variants            - _


const encodings = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789$_';
const decodings = { '~':0
		,'=':0
		,'$':62
		,'_':63
		,'+':62
		,'-':62
		,'.':62
		,'/':63
		,',':63
};
const u8xor_code_encodings2 = new Uint8Array( 64* 128 );

for( var x = 0; x < 256; x++ ) {
	if( x < 64 ) {
		decodings[encodings[x]] = x;
	}
}

//const u8xor_code_encodings2 = new Uint8Array( 64* 128 );

for( let a = 0; a < 64; a++  ) {
   for( let b = 0; b < encodings.length; b++  ) {
     u8xor_code_encodings2[(a<<7)+encodings.codePointAt(b)] = a^b;
   }
}

Object.freeze( decodings );
//Object.freeze( u8xor_code_encodings2 );


function SaltyRNG(f, opt) {

	const readBufs = [];
	const K12_SQUEEZE_LENGTH = 32768;

	const k12buf2 = KangarooTwelveJS();

	function MASK_TOP_MASK(length) {
		return (0xFF) >>> (8 - (length))
	}
	function MY_MASK_MASK(n, length) {
		return (MASK_TOP_MASK(length) << ((n) & 0x7)) & 0xFF;
	}
	function MY_GET_MASK(v, n, mask_size) {
		return (v[(n) >> 3] & MY_MASK_MASK(n, mask_size)) >>> (((n)) & 0x7)
	}

	function compute(buf) {
		k12buf2.update(buf);		
		k12buf2.final();
		return k12buf2.squeeze( K12_SQUEEZE_LENGTH>>3 );
	}
	var RNG = {
		getSalt: f,
		feed(buf) {
			//if( typeof buf === "string" )
			//	buf = toUTF8Array( buf );
			k12buf2.update(buf);
		},
		saltbuf: [],
		entropy: null,
		available: 0,
		used: 0,
		total_bits : 0,
		initialEntropy : null,//"test",
		save() {
			return {
				saltbuf: this.saltbuf.slice(0),
				entropy: this.entropy?this.entropy.slice(0):null,
				available: this.available,
				used: this.used,
				state : k12buf2.clone()
			}
		},
		restore(oldState) {
			this.saltbuf = oldState.saltbuf.slice(0);
			this.entropy = oldState.entropy?oldState.entropy.slice(0):null;
			this.available = oldState.available;
			this.used = oldState.used;
			//throw new Error( "RESTORE STATE IS BROKEN." );
			k12buf2 && k12buf2.copy( oldState.state );
		},
		reset() {
			this.entropy = 
				this.initialEntropy
					?compute(this.initialEntropy)
					:null;
			this.available = 0;
			this.used = 0;
			this.total_bits = 0;
			k12buf2.init();
		},
		getByte() {
			if( this.used & 0x7 ) {
				const buf = this.getBuffer_(8).u8;
				const val = buf[0];
				readBufs[8].push( buf );
				return val;
		
			} else {
				if(this.available === this.used)
					needBits();
				this.total_bits += 8;
				var result = this.entropy[(this.used) >> 3];
				this.used += 8;
				return result;
			}
		},
		getBits(count, signed) {
			if( !count ) { count = 32; signed = true; } 
			if (count > 32)
				throw "Use getBuffer for more than 32 bits.";
			var tmp = this.getBuffer_(count);
			if( !tmp.u32 ) tmp.u32 = new Uint32Array(tmp.ab);
			var val = tmp.u32[0];
			if( signed ) {
				if(  val & ( 1 << (count-1) ) ) { // sign extend
					var negone = ~0;
					negone <<= (count-1);
					val |= negone;
				}
			}
			readBufs[count].push( tmp );
			return val;
		},
		getBuffer(bits) {
			return this.getBuffer_(bits).u8;
		},
		getBuffer_(bits) {
			let resultIndex = 0;
			let resultBits = 0;
			if( readBufs.length <= bits ) { for( let zz = readBufs.length; zz <= bits; zz++ ) readBufs.push([]); }
			let resultBuffer = readBufs[bits].length?readBufs[bits].pop():{ab:new ArrayBuffer(4 * ((bits + 31) >> 5)),u8:null,u32:null};
			let result = resultBuffer.u8?resultBuffer.u8:(resultBuffer.u8 = new Uint8Array(resultBuffer.ab) );
			//result.ab = resultBuffer.ab;
			for( let zz = 0; zz < result.length; zz++ ) result[zz] = 0;
			this.total_bits += bits;
			{
				let tmp;
				let partial_tmp;
				let partial_bits = 0;
				let get_bits;

				do {
					if (bits > 8)
						get_bits = 8;
					else
						get_bits = bits;
					// if there were 1-7 bits of data in partial, then can only get 8-partial max.
					if( (8-partial_bits) < get_bits )
						get_bits = (8-partial_bits);
					// if get_bits == 8
					//    but bits_used is 1-7, then it would have to pull 2 bytes to get the 8 required
					//    so truncate get_bits to 1-7 bits
					let chunk = ( 8 - ( this.used & 7) );
					if( chunk < get_bits )
						get_bits = chunk;
					// if resultBits is 1-7 offset, then would have to store up to 2 bytes of value
					//    so have to truncate to just the up to 1 bytes that will fit.
					chunk = ( 8 - ( resultBits & 7) );
					if( chunk < get_bits )
						get_bits = chunk;

					//console.log( "Get bits:", get_bits, " after", this.used, "into", resultBits );
					// only greater... if equal just grab the bits.
					if (get_bits > (this.available - this.used)) {
						if (this.available - this.used) {
							partial_bits = this.available - this.used;
							// partial can never be greater than 8; request is never greater than 8
							//if (partial_bits > 8)
							//	partial_bits = 8;
							partial_tmp = MY_GET_MASK(this.entropy, this.used, partial_bits);
						}
						needBits();
						bits -= partial_bits;
					}
					else {
						tmp = MY_GET_MASK(this.entropy, this.used, get_bits);
						this.used += get_bits;
						if (partial_bits) {
							tmp = partial_tmp | (tmp << partial_bits);
							partial_bits = 0;
						}
						
						result[resultIndex] |= tmp << (resultBits&7);
						resultBits += get_bits;
						// because of input limits, total result bits can only be 8 or less.
						if( resultBits == 8 ) {
							resultIndex++;
							resultBits = 0;
						}
						bits -= get_bits;
					}
				} while (bits);
				//console.log( "output is ", result[0].toString(16), result[1].toString(16), result[2].toString(16), result[3].toString(16) )
				return resultBuffer;
			}
		}
	};
	function needBits() {
		RNG.saltbuf.length = 0;
			if (typeof (RNG.getSalt) === 'function') {
				RNG.getSalt(RNG.saltbuf);
                        	if( RNG.entropy ) {
					k12buf2.init();
					k12buf2.update( RNG.entropy.slice( 0, 200 ) );
                                }
				if( RNG.saltbuf.length ) {
					k12buf2.update( RNG.saltbuf );
				}
			}

		if( k12buf2.squeezing() ) {
			RNG.entropy = k12buf2.squeeze(K12_SQUEEZE_LENGTH>>3); // customization is a final pad string.
		}else console.log( "Not squeezing so all is bad?" );
		RNG.available = RNG.entropy.length * 8;
		RNG.used = 0;
	}	
	RNG.reset();
	return RNG;
}


//import {  keccakprg } from './standalone/sha3-addons.js';

/*! noble-hashes - MIT License (c) 2022 Paul Miller (paulmillr.com) */
/*! Salty Random Generator - Extracted KeccakPRG and support functions from at least 1.0 tag 
  - d3x0r
*/

const isLE = new Uint8Array(new Uint32Array([0x11223344]).buffer)[0] === 0x44;
// There is almost no big endian hardware, but js typed arrays uses platform specific endianess.
// So, just to be sure not to corrupt anything.
if (!isLE)
    throw new Error('Non little-endian hardware is not supported');
/**
 * @example bytesToHex(Uint8Array.from([0xde, 0xad, 0xbe, 0xef]))
 */
function utf8ToBytes(str) {
    if (typeof str !== 'string') {
        throw new TypeError(`utf8ToBytes expected string, got ${typeof str}`);
    }
    return new TextEncoder().encode(str);
}

function toBytes(data) {
    if (typeof data === 'string')
        data = utf8ToBytes(data);
    if (!(data instanceof Uint8Array))
        throw new TypeError(`Expected input type is Uint8Array (got ${typeof data})`);
    return data;
}

function assertNumber(n) {
    if (!Number.isSafeInteger(n) || n < 0)
        throw new Error(`Wrong positive integer: ${n}`);
}
const u32 = (arr) => new Uint32Array(arr.buffer, arr.byteOffset, Math.floor(arr.byteLength / 4));
//import { toBytes, wrapConstructorWithOpts, assertNumber, u32, } from './utils.js';


class Hash {
    // Safe version that clones internal state
}
function assertBytes(bytes, ...lengths) {
    if (!(bytes instanceof Uint8Array))
        throw new TypeError('Expected Uint8Array');
    if (lengths.length > 0 && !lengths.includes(bytes.length))
        throw new TypeError(`Expected Uint8Array of length ${lengths}, not of length=${bytes.length}`);
}
function assertExists(instance, checkFinished = true) {
    if (instance.destroyed)
        throw new Error('Hash instance has been destroyed');
    if (checkFinished && instance.finished)
        throw new Error('Hash#digest() has already been called');
}
function assertOutput(out, instance) {
    _assert && assertBytes(out);
    const min = instance.outputLen;
    if (out.length < min) {
        throw new Error(`digestInto() expects output buffer of length at least ${min}`);
    }
}



// import * as u64 from './_u64.js';

const [SHA3_PI, SHA3_ROTL, _SHA3_IOTA] = [[], [], []];
const _0n = BigInt(0);
const _1n = BigInt(1);
const _2n = BigInt(2);
const _7n = BigInt(7);
const _256n = BigInt(256);
const _0x71n = BigInt(0x71);
const security = 128;
const capacity = (2*security);

for (let round = 0, R = _1n, x = 1, y = 0; round < 24; round++) {
    // Pi
    [x, y] = [y, (2 * x + 3 * y) % 5];
    SHA3_PI.push(2 * (5 * y + x));
    // Rotational
    SHA3_ROTL.push((((round + 1) * (round + 2)) / 2) % 64);
    // Iota
    let t = _0n;
    for (let j = 0; j < 7; j++) {
        R = ((R << _1n) ^ ((R >> _7n) * _0x71n)) % _256n;
        if (R & _2n)
            t ^= _1n << ((_1n << BigInt(j)) - _1n);
    }
    _SHA3_IOTA.push(t);
}

const SHA3_IOTA_H = new Uint32Array(_SHA3_IOTA.length);
const SHA3_IOTA_L = new Uint32Array(_SHA3_IOTA.length); 
for (let i = 0; i < _SHA3_IOTA.length; i++) {
		SHA3_IOTA_H[i] = Number((_SHA3_IOTA[i]) & (2n ** 32n - 1n))
		SHA3_IOTA_L[i] = Number(((_SHA3_IOTA[i]) >> 32n) & (2n ** 32n - 1n))
}


function rightEncodeK12(n) {
    const res = [];
    for (; n > 0; n >>= 8)
        res.unshift(n & 0xff);
    res.push(res.length);
    return new Uint8Array(res);
}

class Keccak extends Hash {
    // NOTE: we accept arguments in bytes instead of bits here.
    constructor(blockLen, suffix, outputLen, enableXOF = false, rounds = 12) {
        super();
        this.blockLen = blockLen;
        this.suffix = suffix;
        this.outputLen = outputLen;
        this.enableXOF = enableXOF;
        this.rounds = rounds;
        this.pos = 0;
        this.posOut = 0;
        this.finished = false;
        this.destroyed = false;
        // Can be passed from user as dkLen
        _assert && assertNumber(outputLen);
        // 1600 = 5x5 matrix of 64bit.  1600 bits === 200 bytes
        if (0 >= this.blockLen || this.blockLen >= 200)
            throw new Error('Sha3 supports only keccak-f1600 function');
        this.state = new Uint8Array(200);
        this.state32 = u32(this.state);
    }
    keccak() {
        //keccakP(this.state32, this.rounds); 
        const s = this.state32;
        const rounds = this.rounds;
        const B = new Uint32Array(5 * 2);
        // NOTE: all indices are x2 since we store state as u32 instead of u64 (bigints to slow in js)
        for (let round = 24 - rounds; round < 24; round++) {
            // Theta θ
            for (let x = 0; x < 10; x++)
                B[x] = s[x] ^ s[x + 10] ^ s[x + 20] ^ s[x + 30] ^ s[x + 40];
            for (let x = 0; x < 10; x += 2) {
                const idx1 = (x + 8) % 10;
                const idx0 = (x + 2) % 10;
                const B0 = B[idx0];
                const B1 = B[idx0 + 1];  
                const Th = ((B0 << 1) | (B1 >>> (31)))^ B[idx1];     //  u64.rotlSH(B0, B1, 1)^ B[idx1];    // rotlH(B0, B1, 1) ^ B[idx1];
                const Tl = ((B1 << 1) | (B0 >>> (31)))^ B[idx1 + 1]; // u64.rotlSL(B0, B1, 1)^ B[idx1 + 1];// rotlL(B0, B1, 1) ^ B[idx1 + 1];
                for (let y = 0; y < 50; y += 10) {
                    s[x + y] ^= Th;
                    s[x + y + 1] ^= Tl;
                }
            }
            // Rho (ρ) and Pi (π)
            let curH = s[2];
            let curL = s[3];
            for (let t = 0; t < 24; t++) {
                const shift = SHA3_ROTL[t];
                const Th = shift > 32 ? ((curL << (shift - 32)) | (curH >>> (64 - shift))) : ((curH << shift) | (curL >>> (32 - shift))); //   rotlH(curH, curL, shift);
                const Tl = shift > 32 ? ((curH << (shift - 32)) | (curL >>> (64 - shift))) : ((curL << shift) | (curH >>> (32 - shift))); //   u64.rotlBL(curH, curL, shift) : u64.rotlSL(curH, curL, shift)  ;//rotlL(curH, curL, shift);
                const PI = SHA3_PI[t];
                curH = s[PI];
                curL = s[PI + 1];
                s[PI] = Th;
                s[PI + 1] = Tl;
            }
            // Chi (χ)
            for (let y = 0; y < 50; y += 10) {
                for (let x = 0; x < 10; x++)
                    B[x] = s[y + x];
                for (let x = 0; x < 10; x++)
                    s[y + x] ^= ~B[(x + 2) % 10] & B[(x + 4) % 10];
            }
            // Iota (ι)
            s[0] ^= SHA3_IOTA_H[round];
            s[1] ^= SHA3_IOTA_L[round];
        }
        //B.fill(0);


        this.posOut = 0;
        this.pos = 0;
    }
    update(data) {
        _assert && assertExists(this);
        const { blockLen, state } = this;
        data = toBytes(data);
        const len = data.length;
        for (let pos = 0; pos < len;) {
            const take = Math.min(blockLen - this.pos, len - pos);
            for (let i = 0; i < take; i++)
                state[this.pos++] ^= data[pos++];
            if (this.pos === blockLen) {
                this.keccak();
				}
        }
        return this;
    }
    finish() {
        if (this.finished) {
				console.log( "is already finished??" );
            return;
			}
        this.finished = true;
        const { state, suffix, pos, blockLen } = this;
        // Do the padding
        state[pos] ^= suffix;
        if ((suffix & 0x80) !== 0 && pos === blockLen - 1)
            this.keccak();
        state[blockLen - 1] ^= 0x80;
        this.keccak();
    }
    writeInto(out) {
        _assert && assertExists(this, false);
        _assert && assertBytes(out);
        this.finish();
        const bufferOut = this.state;
        const { blockLen } = this;
        for (let pos = 0, len = out.length; pos < len;) {
            if (this.posOut >= blockLen) {
                this.keccak();
				}
            const take = Math.min(blockLen - this.posOut, len - pos);
            out.set(bufferOut.subarray(this.posOut, this.posOut + take), pos);
            this.posOut += take;
            pos += take;
        }
        return out;
    }
    xofInto(out) {
        // Sha3/Keccak usage with XOF is probably mistake, only SHAKE instances can do XOF
        if (!this.enableXOF)
            throw new Error('XOF is not possible for this instance');
        return this.writeInto(out);
    }
    xof(bytes) {
        _assert && assertNumber(bytes);
        return this.xofInto(new Uint8Array(bytes));
    }
    digestInto(out) {
        _assert && assertOutput(out, this);
        if (this.finished)
            throw new Error('digest() was already called');
        this.writeInto(out);
        this.destroy();
        return out;
    }
    digest() {
        return this.digestInto(new Uint8Array(this.outputLen));
    }
    destroy() {
        this.destroyed = true;
        this.state.fill(0);
    }
}

const toBytesOptional = (buf) => (buf !== undefined ? (0, toBytes)(buf) : new Uint8Array([]));

class KangarooTwelve extends Keccak {
    constructor(blockLen, leafLen, outputLen, rounds, opts) {
        super(blockLen, 0x07, outputLen, true, rounds);
        this.leafLen = leafLen;
        this.chunkLen = 8192;
        this.chunkPos = 0; // Position of current block in chunk
        this.chunksDone = 0; // How many chunks we already have
        const { personalization } = opts;
        this.personalization = toBytesOptional(personalization);
    }
    update(data) {
        data = (0, toBytes)(data);
        const { chunkLen, blockLen, leafLen, rounds } = this;
        for (let pos = 0, len = data.length; pos < len;) {
            if (this.chunkPos == chunkLen) {
                if (this.leafHash)
                    super.update(this.leafHash.digest());
                else {
                    this.suffix = 0x06; // Its safe to change suffix here since its used only in digest()
                    super.update(new Uint8Array([3, 0, 0, 0, 0, 0, 0, 0]));
                }
                this.leafHash = new Keccak(blockLen, 0x0b, leafLen, false, rounds);
                this.chunksDone++;
                this.chunkPos = 0;
            }
            const take = Math.min(chunkLen - this.chunkPos, len - pos);
            const chunk = data.subarray(pos, pos + take);
            if (this.leafHash)
                this.leafHash.update(chunk);
            else
                super.update(chunk);
            this.chunkPos += take;
            pos += take;
        }
        return this;
    }
    finish() {
        if (this.finished)
            return;
        const { personalization } = this;
        this.update(personalization).update(rightEncodeK12(personalization.length));
        // Leaf hash
        if (this.leafHash) {
            super.update(this.leafHash.digest());
            super.update(rightEncodeK12(this.chunksDone));
            super.update(new Uint8Array([0xff, 0xff]));
        }
        super.finish.call(this);
    }
    destroy() {
        super.destroy.call(this);
        if (this.leafHash)
            this.leafHash.destroy();
        // We cannot zero personalization buffer since it is user provided and we don't want to mutate user input
        this.personalization = EMPTY;
    }
}



function KangarooTwelveJS() {
	const data = {
		k : 0,
		keybuf : 0,
		keybuflen : 0,
		buf : 0,
		bufMaps : new WeakMap(),
		outbuf : 0,
		realBuf : null,
	};
	let phase = 1;
	data.k = new KangarooTwelve((1600-capacity) / 8, 0, 0, 12, {});
	var K12 = {
		init() {
			//data.k.forget()
			data.k = new KangarooTwelve((1600-capacity) / 8, 0, 0, 12, {});			
		},
		drop() {
			data.keybuf = null;
			data.buf = null;
			data.k = null;
			//console.log( "S?", s );
		},
		update(buf) {
			phase = 2;
			if( buf instanceof Array ) {
				if( "number" === typeof buf[0] ) {
					//buf = buf.join();
					//console.log( "xxBuf join?", buf );
					const byteLength = buf.length;
					const newbuf = new Uint8Array(byteLength );
					for( let n = 0; n < buf.length; n++ ) newbuf[n]=buf[n];
					buf = newbuf;
					//console.log( "yay?", byteLength, buf );
				} else {
					buf = buf.join();
					//console.log( "Buf join?", buf );
				}
			} 
			data.k.update( buf );
		},
		final() {
			//data.k.final();
		},
		squeeze(n) {			
			return data.k.xof(n);//data.k.fetch( n );
		},
		release(buf) {
		},
		absorbing() {
			if( phase === 1 ) return true;
			return false;
		},
		squeezing() {
			if( phase === 2 ) return true;
			return false;				
		},
		clone() {
                    console.log( "clone not implemented?" );
		},
		copy(from) {
                    console.log( "copy not implemented?" );
		},
		phase() {
			return phase;
		},
	};
	
	//data.k = k12._NewKangarooTwelve();
	//data.outbuf = k12._malloc( 64 );
	//console.log( "malloc:", data.outbuf );
	//data.realBuf = k12.HEAPU8.slice( data.outbuf, data.outbuf+64 );
	//data.realBuf = new Uint8Array( k12.HEAPU8.buffer, data.outbuf, 64 );
	//K12.absorbing = k12._KangarooTwelve_IsAbsorbing.bind(k12,data.k),
	//K12.squeezing = k12._KangarooTwelve_IsSqueezing.bind(k12,data.k),

	K12.init();

	return K12;
}

//-------------- byte xbox ------------------------------

function BlockShuffle_ByteShuffler( ctx ) {
	//struct byte_shuffle_key *key = New( struct byte_shuffle_key );
	var key = { map:[], dmap:[] };
	var n;
	for( n = 0; n < 256; n++ )
		key.map[n] = n;

	// simple-in-place shuffler.

	for( n = 0; n < 256; n++ ) {
		var m;
		var t;
		m = ctx.getByte(); //console.log( "swap:", n, m );
		t = key.map[m];
		key.map[m] = key.map[n];
		key.map[n] = t;
	}

	for( n = 0; n < 256; n++ )
		key.dmap[key.map[n]] = n;
	return key;
}

//----------------- K12-XBOX Utilities -----------------------------

// bit size of masking hash.
const RNGHASH = 256;
const localCiphers = [];
function SRG_XSWS_encryptData( objBuf, tick, keyBuf ) {
	if( objBuf.buffer.byteLength & 0x7 ) {
		throw new Error( "buffer to encode must be a multiple of 64 bits; (should also include last byte of padding specification)" );
	}
	function encryptBlock( bytKey
		, output, output8, offset, outlen 
		,  bufKey
	) {
		var n;
		var dolen = outlen/4;
		for( n = 0; n < dolen; n++ ) output[n] ^= bufKey [ ((n) % (RNGHASH / 32)) ];
		var p = 0x55;
		for( n = 0; n < outlen; n++ )  p = output8[n] = bytKey.map[output8[n] ^ p];
		p = 0xAA;
		for( n = outlen-1; n >= 0; n-- ) p = output8[n] = bytKey.map[output8[n] ^ p];
	}

	var signEntropy = localCiphers.pop();
	if( !signEntropy ) {
		signEntropy = SaltyRNG( null);
		signEntropy.initialEntropy = null;
	}
	signEntropy.reset();	
	signEntropy.feed( tick );
	signEntropy.feed( keyBuf );

	var bufKey = new Uint32Array( signEntropy.getBuffer( RNGHASH ) );
	var bytKey = BlockShuffle_ByteShuffler( signEntropy );

	var outBufLen = objBuf.length * 4;
	//outBuf[0] = (uint8_t*)HeapAllocateAligned( NULL, (*outBufLen), 4096 );
	var outBuf = objBuf;//new Uint32Array( outBufLen / 4 );
	var outBuf8 = new Uint8Array( outBuf.buffer );

	for( var b = 0; b < outBufLen; b += 4096 ) {
		var bs = outBufLen - b;
		if( bs > 4096 )
			encryptBlock( bytKey, outBuf, outBuf8, b, 4096, bufKey );
		else
			encryptBlock( bytKey, outBuf, outBuf8, b, bs, bufKey );
	}

	localCiphers.push( signEntropy );

	return outBuf8;
}

function SRG_XSWS_encryptString( objBuf, tick, keyBuf ) {
	var tickBuf = new Uint32Array( 2 );
	
	tickBuf[0] = tick & 0xFFFFFFFF;
	tickBuf[1] = ( tick / 0x100000000 ) & 0xFFFFFFFF;
	var ob = myTextEncoder( objBuf );
	//console.log( "INPUT BUF?", ob.length );
	var ob32 = new Uint32Array( ob.buffer );
	//console.log( "BUF?", ob32, ob32[0], tickBuf, keyBuf );
	return SRG_XSWS_encryptData( ob32, tickBuf, keyBuf );
}

function SRG_XSWS_decryptData( objBuf,  tick, keyBuf ) {
	function decryptBlock( bytKey
		, input, offset,  len
		, output, output8
		, bufKey
	) {
		var n;
		for( n = 0; n < (len - 1); n++ ) output8[offset+n] = bytKey.dmap[input[offset+n]] ^ input[offset+n+1];
        	output8[offset+n] = bytKey.dmap[output[offset+n]] ^ 0xAA;
		for( n = (len - 1); n > 0; n-- ) output8[offset+n] = bytKey.dmap[output8[offset+n]] ^ output8[offset+n-1];
		output8[offset+0] = bytKey.dmap[output8[offset+0]] ^ 0x55;
		var dolen = len / 4;
		for( n = 0; n < dolen; n ++ ) output[offset+n] ^= bufKey [ ((n) % (RNGHASH / 32)) ];
	}

	
	var signEntropy = localCiphers.pop();
	if( !signEntropy ) {
		signEntropy = SaltyRNG( null);
		signEntropy.initialEntropy = null;
	}
	
	signEntropy.reset();	
	signEntropy.feed( tick );
	signEntropy.feed( keyBuf );

	var bufKey = new Uint32Array( signEntropy.getBuffer( RNGHASH ) );
	var bytKey = BlockShuffle_ByteShuffler( signEntropy );

	var outBuf = new Uint32Array( objBuf.length );
	var outBuf8 = new Uint8Array( outBuf.buffer );
	var blockLen = objBuf.buffer.byteLength;
	for( var b = 0; b < blockLen; b += 4096 ) {
		var bs = blockLen - b;
		if( bs > 4096 )
			decryptBlock( bytKey, objBuf, b, 4096, outBuf, outBuf8, bufKey );
		else
			decryptBlock( bytKey, objBuf, b, bs, outBuf, outBuf8, bufKey );
	}
	outBuf = new Uint8Array( outBuf8, 0, outBuf.length - outBuf[0] + objBuf.buffer.length - 1 );

	localCiphers.push( signEntropy );
	return outBuf;
}

function SRG_XSWS_decryptString( objBuf, tick, keyBuf ) {
	var tickBuf = new Uint32Array( 2 );
	
	tickBuf[0] = tick & 0xFFFFFFFF;
	tickBuf[1] = ( tick / 0x100000000 ) & 0xFFFFFFFF;
	var outBuf = SRG_XSWS_decryptData( objBuf, tickBuf, keyBuf );
	return myTextDecoder( outBuf );
}

        // string->Uint8
	function myTextEncoder(s) {	
		var chars = [...s];
		var len = 0;
		for( var n = 0; n < chars.length; n++ ) {
			var chInt = chars[n].codePointAt(0);
			if( chInt < 128 ) 
				len++;
			else if( chInt < 0x800 ) 
				len += 2;
			else if( chInt < 0x10000 ) 
				len += 3;
			else if( chInt < 0x110000 ) 
				len += 4;
		}
		len+=2;
		var out = new Uint8Array( len + ( len&7?(8-(len&7)):0 ) );
		len = 0;			
		for( var n = 0; n < chars.length; n++ ) {
			var chInt = chars[n].codePointAt(0);
			if( chInt < 128 ) 
				out[len++] = chInt;
			else if( chInt < 0x800 ) {
				out[len++] = ( (chInt & 0x7c0) >> 6 ) | 0xc0;
				out[len++] = ( (chInt & 0x03f) ) | 0x80;
			} else if( chInt < 0x10000 ) {
				out[len++] = ( (chInt & 0xf000) >> 12 ) | 0xE0;
				out[len++] = ( (chInt & 0x0fc0) >> 6 ) | 0x80;
				out[len++] = ( (chInt & 0x003f) ) | 0x80;
			} else if( chInt < 0x110000 ) {
				out[len++] = ( (chInt & 0x01c0000) >> 18 ) | 0xF0;
				out[len++] = ( (chInt & 0x003f000) >> 12 ) | 0xE0;
				out[len++] = ( (chInt & 0x0000fc0) >> 6 ) | 0x80;
				out[len++] = ( (chInt & 0x000003f) ) | 0x80;
			}
		}
		out[len] = 0xFF;
		out[out.length-1] = out.length - (len);
		return out;
	}
        // uInt8 ->string
	function myTextDecoder(buf) {
		var out = '';
		for( var n = 0; n < buf.length; n++ ) {
			if( buf[n] === 0xFF ) break;
			if( ( buf[n]& 0x80 ) == 0 )
				out += String.fromCodePoint( buf[n] );
			else if( ( buf[n] & 0xC0 ) == 0x80 ) ; else if( ( buf[n] & 0xE0 ) == 0xC0 ) {
				out += String.fromCodePoint( ( ( buf[n] & 0x1f ) << 6 ) | ( buf[n+1] & 0x3f ) );
				n++;
			} else if( ( buf[n] & 0xF0 ) == 0xE0 ) {
				out += String.fromCodePoint( ( ( buf[n] & 0xf ) << 12 ) | ( ( buf[n+1] & 0x3f ) << 6 ) | ( buf[n+2] & 0x3f ) );
				n+=2;
			} else if( ( buf[n] & 0xF8 ) == 0xF0 ) {
				out += String.fromCodePoint( ( ( buf[n] & 0x7 ) << 18 ) | ( ( buf[n+1] & 0x3f ) << 12 ) | ( ( buf[n+2] & 0x3f ) << 6 ) | ( buf[n+3] & 0x3f ) );
				n+=3;
			} else if( ( buf[n] & 0xFC ) == 0xF8 ) {
				out += String.fromCodePoint( ( ( buf[n] & 0x3 ) << 24 ) | ( ( buf[n+1] & 0x3f ) << 18 ) | ( ( buf[n+2] & 0x3f ) << 12 ) | ( ( buf[n+3] & 0x3f ) << 6 ) | ( buf[n+4] & 0x3f ) );
				n+=4;
			}
		}
		return out;
	}

function GetCurrentTick() {
	var now = new Date();
	var tick = now.getTime() * 256;
	tick |= ( -now.getTimezoneOffset() /15 ) & 0xFF;
	return tick;
}


function TickToTime( tick ) {
}



const seeds = [];
function shuffleSeeder(salt){
  var val;
  if( seeds.length ) {
    //console.log( "using seed... ", seeds.length )
    salt.push( seeds.shift() );
  } else {
    salt.push(  ( val = new Date().getTime(), val =( val % 100000 ) * ( val % 100000 ) )  );
    // save seeds to rebuild stream later is possible...
    //if( outSeeds )
    //  outSeeds.write( String(val) + "\n");
  }
}

function Holder() {
  return {
    number : 0
    , r : 0
    , less : null
    , more : null
  };
}
var holders = [];

function sort(  tree,  number,  r )
{
    //console.log( "Assign ", r, " to ", number)
   if( !tree )
   {
      tree = holders.pop();
      if( !tree ) tree = Holder();
      tree.number = number;
      tree.r = r;
      tree.pLess = tree.pMore = null;
   }
   else
   {
      if( r > tree.r )
         tree.pMore = sort( tree.pMore, number, r );
      else
         tree.pLess = sort( tree.pLess, number, r );
   }
   return tree;
}
function  FoldTree( tree, numbers, count )
{
   if( !(count-numbers.length) ) return numbers;
   if( tree.pLess )
      FoldTree( tree.pLess, numbers, count );
   numbers.push(tree.number);
   holders.push(tree);
   if( tree.pMore )
      FoldTree( tree.pMore, numbers, count ); 
   return numbers
}

function  Shuffle( numbers, count, RNG )
{
	const bits = (Math.ceil(Math.log2( numbers.length ))+2 )|0;
	var tree;
	var n;
	tree = null;
	for( n of numbers )
		tree = sort( tree, n, RNG.getBits(bits) );//RNG.getBits( 13 ) );

	var x = FoldTree( tree, [], count||numbers.length );
	
	return x;
}

function Shuffler( opts ) {
	var RNG;
	if( opts && opts.salt ) {
		RNG = SaltyRNG( opts.salt);
	}
	else 
		RNG = SaltyRNG( shuffleSeeder);
	return {
		shuffle(numbers,count) {
			 return Shuffle(numbers,count, RNG);
		}
	};
}

SaltyRNG.Shuffler = Shuffler;

//----------------------------------------------------------------------------

const RNG= SaltyRNG( 
	(saltbuf)=>saltbuf.push( new Date().toISOString() ));
const RNG2 = SaltyRNG( getSalt2);
RNG2.initialEntropy = null;


let salt = null;
function getSalt2 (saltbuf) {
    if( salt ) {
        saltbuf.push( salt );
        salt = null;
    }
}

SaltyRNG.id = function( s ) {
	if( s !== undefined ) {
		salt = s;
		RNG2.reset();
		//RNG2.feed( "\0\0\0\0");
		//RNG2.feed( s );
		// this is an ipv6 + UUID
		return base64ArrayBuffer( RNG2.getBuffer(8*(16+16)) );
	}
   	return base64ArrayBuffer( RNG.getBuffer(8*(16+16)) );
};

SaltyRNG.Id = function(s) {
    // this is an ipv6 + UUID
    let ID;
	if( s !== undefined ) {
		salt = s;
		RNG2.reset();
		// this is an ipv6 + UUID
		ID = RNG2.getBuffer(8*(12));
	}
	else {
    		ID = RNG.getBuffer(8*(12));
                // 1 second marker
	    const now = ( Date.now() / 1000 ) | 0;
	    ID[0] = ( now & 0xFF0000 ) >> 16;
	    ID[1] = ( now & 0x00FF00 ) >> 8;
	    ID[2] = ( now & 0x0000FF );
	}
    return base64ArrayBuffer( ID );
};

SaltyRNG.u16_id = function() {
    // this is an ipv6 + UUID
    var out = [];
    for( var c = 0; c < 25; c++ ) {
    	var ch = RNG.getBits( 10 ); if( ch < 32 ) ch |= 64;
    	out[c] = String.fromCodePoint( ch );
    }
    return out.join('');
};

function signCheck( buf ) {
		buf = new Uint8Array(buf);
		var n, b;
		var is0 = 0;
		var is1 = 0;
		var long0 = 0;
		var long1 = 0;
		var longest0 = 0;
		var longest1 = 0;
		var ones = 0;
		for( n = 0; n < 32; n++ ) {
			for( b = 0; b < 8; b++ ) {
				if( buf[n] & (1 << b) ) {
					ones++;
					if( is1 ) {
						long1++;
					}
					else {
						if( long0 > longest0 ) longest0 = long0;
						is1 = 1;
						is0 = 0;
						long1 = 1;
					}
				}
				else {
					if( is0 ) {
						long0++;
					}
					else {
						if( long1 > longest1 ) longest1 = long1;
						is0 = 1;
						is1 = 0;
						long0 = 1;
					}
				}
			}
		}
// 167-128 = 39 = 40+ dif == 30 bits in a row approx
//const overbal = (167-128)
		const overbal = (167-128);
                    //console.log( "result:", overbal, longest0, longest1, ones );
		if( longest0 > 29 || longest1 > 29 || ones > (128+overbal) || ones < (128-overbal) ) {
			return 1;
		}
		return 0;
	}

let signEntropy;
let nextSalt = new Uint8Array(32);
SaltyRNG.sign = function( msg ) {

		//SRGObject *obj = ObjectWrap::Unwrap<SRGObject>( args.This() );
		var id;
		//memcpy( nextSalt, *buf, buf.length() );
		if( !signEntropy ) {
			signEntropy = SaltyRNG( null);
			signEntropy.initialEntropy = null;
		}

		do {
			signEntropy.reset();
			//console.log( "Feed message", msg );
			signEntropy.feed( msg );

			{
				id = SaltyRNG.id();
				DecodeBase64Into( nextSalt, id );
				signEntropy.feed( nextSalt );
				var bytes = signEntropy.getBuffer( 256 );
				if( signCheck( bytes ) ) ; else {
					id = null;
				}
			}
		} while( !id );
		return id;
		
};

SaltyRNG.verify = function( msg, id  ) {
		if( !signEntropy ) {
			signEntropy = SaltyRNG( null);
			signEntropy.initialEntropy = null;
		}
		signEntropy.reset();
		//console.log( "Feed message.", msg );
		signEntropy.feed( msg );
		DecodeBase64Into( nextSalt, id );
		//console.log( "Feed ID", nextSalt, id );
		signEntropy.feed( nextSalt );
		var bytes = signEntropy.getBuffer( 256 );
		//console.log( "bytes:", new Uint8Array( bytes ) );
		return signCheck( bytes );
};


function base64ArrayBuffer(arrayBuffer) {
  var base64    = '';

  var bytes         = new Uint8Array(arrayBuffer);
  var byteLength    = bytes.byteLength;
  var byteRemainder = byteLength % 3;
  var mainLength    = byteLength - byteRemainder;

  var a, b, c, d;
  var chunk;
  //throw "who's using this?"
  //console.log( "buffer..", arrayBuffer )
  // Main loop deals with bytes in chunks of 3
  for (var i = 0; i < mainLength; i = i + 3) {
    // Combine the three bytes into a single integer
    chunk = (bytes[i] << 16) | (bytes[i + 1] << 8) | bytes[i + 2];

    // Use bitmasks to extract 6-bit segments from the triplet
    a = (chunk & 16515072) >> 18; // 16515072 = (2^6 - 1) << 18
    b = (chunk & 258048)   >> 12; // 258048   = (2^6 - 1) << 12
    c = (chunk & 4032)     >>  6; // 4032     = (2^6 - 1) << 6
    d = chunk & 63;               // 63       = 2^6 - 1

    // Convert the raw binary segments to the appropriate ASCII encoding
    base64 += encodings[a] + encodings[b] + encodings[c] + encodings[d];
  }

  // Deal with the remaining bytes and padding
  if (byteRemainder == 1) {
    chunk = bytes[mainLength];
    a = (chunk & 252) >> 2; // 252 = (2^6 - 1) << 2
    // Set the 4 least significant bits to zero
    b = (chunk & 3)   << 4; // 3   = 2^2 - 1
    base64 += encodings[a] + encodings[b] + '==';
  } else if (byteRemainder == 2) {
    chunk = (bytes[mainLength] << 8) | bytes[mainLength + 1];
    a = (chunk & 64512) >> 10; // 64512 = (2^6 - 1) << 10
    b = (chunk & 1008)  >>  4; // 1008  = (2^6 - 1) << 4
    // Set the 2 least significant bits to zero
    c = (chunk & 15)    <<  2; // 15    = 2^4 - 1
    base64 += encodings[a] + encodings[b] + encodings[c] + '=';
  }
  //console.log( "dup?", base64)
  return base64
}


function DecodeBase64Into( out, buf ) {
	var outsize = 0;
	// if the buffer is truncated in length, use that as the 
	// constraint, and if 1 char results with 6 bits, do not
	// count that as a whole byte of output.
        if( !out ) {
		if( buf.length % 4 == 1 )
			outsize = (((buf.length + 3) / 4) * 3) - 3;
		else if( buf.length % 4 == 2 )
			outsize = (((buf.length + 3) / 4) * 3) - 2;
		else if( buf.length % 4 == 3 )
			outsize = (((buf.length + 3) / 4) * 3) - 1;
		else if( buf[buf.length - 1] == '=' ) {
			if( buf[buf.length - 2] == '=' )
				outsize = (((buf.length + 3) / 4) * 3) - 2;
			else
				outsize = (((buf.length + 3) / 4) * 3) - 1;
		}
		else
			outsize = (((buf.length + 3) / 4) * 3);
		out = new Uint8Array( outsize );
	}

	var n;
	var l = (buf.length+3)/4;
	for( n = 0; n < l; n++ )
	{
		var index0 = decodings[buf[n*4]];
		var index1 = decodings[buf[n*4+1]];
		var index2 = decodings[buf[n*4+2]];
		var index3 = decodings[buf[n*4+3]];
		
		out[n*3+0] = (( index0 ) << 2 | ( index1 ) >> 4);
		out[n*3+1] = (( index1 ) << 4 | ( ( ( index2 ) >> 2 ) & 0x0f ));
		out[n*3+2] = (( index2 ) << 6 | ( ( index3 ) & 0x3F ));
	}

	return out;
}


// Converts an ArrayBuffer directly to base64, without any intermediate 'convert to string then
// use window.btoa' step. According to my tests, this appears to be a faster approach:
// http://jsperf.com/encoding-xhr-image-data/5
// doesn't have to be reversable....



var xor_code_encodings = {};//'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_'
for( var a = 0; a < encodings.length; a++  ) {
   var r = (xor_code_encodings[encodings[a]]={} );
   for( var b = 0; b < encodings.length; b++  ) {
	r[encodings[b]] = encodings[a^b];
   }
}
xor_code_encodings['='] = {'=': '='};

function xor(a,b) {
  var c = "";
  for( var n = 0; n < a.length; n++ ) {
	c += xor_code_encodings[a[n]][b[n]];
  }
  return c
}
SaltyRNG.xor = xor;

function dexor(a,b,d,e) {
  var r = "";
  var n1 = (d-1)*((a.length/e)|0);
  var n2 = (d)*(a.length/e)|0;

  for( var n = 0; n < n1; n++ )
	r += a[n];
  for( ; n < n2; n++ )
	r += xor_code_encodings[a[n]][b[n]];
  for( ; n < n2; n++ )
	r += a[n];

  return r
}
SaltyRNG.dexor=dexor;

function txor(a,b) {
	const d = [...b].map( (c)=>c.codePointAt(0) );
	const keylen = d.length;
	return [...a].map( (c,n)=>String.fromCodePoint( c.codePointAt(0)^d[n%keylen] )).join("");
}
SaltyRNG.u16xor=txor;

function makeXKey( key, step ) {
    return { key : key, keybuf: key?base64ArrayBuffer(key):null, step: step?step:0
	, setKey(key,step) { this.key = key; this.keybuf = DecodeBase64Into( new Uint8Array(Math.ceil(key.length*3/4)), key); this.step = step?step:0; } };
}

function makeU16Key( ) {
    return SaltyRNG.u16generator();
}

SaltyRNG.xkey = makeXKey;
SaltyRNG.ukey = makeU16Key;


//   0x xx xx xx     7 bits
//   10 xx xx xx   0  continuation byte
//   mask 3f
//   11 0x xx xx   6  8-11 bits
//       X XX X_  0x1E  ( required bits )  0x1 allowed bits on first byte
//   11 10 xx xx   12  12-16 bits
//         XX XX   10 X_ __ __   required bits  0x1F (allowed bits on second)
//   11 11 0x xx   18  17-21 bits
//          X XX   10 XX __ __   0xF (allowed bits on second)
//   11 11 10 xx   24  22-26 bits
//            XX   10 XX X_ __   0x7 (allowed bits on second byte)
//   11 11 11 0x   30  27-31 bits
//             X   10 XX XX __  0x3 (allowed bits on second byte)

function u8xor_node(a,b) {
	let buf = Buffer.from(a, 'utf8');
	if( !b.keybuf ) { /*console.trace( "Key needs buf...." );*/ b.keybuf = Buffer.from( b.key, 'utf8' ); }
	let c = b.keybuf;//Buffer.from(b.key, 'utf8');
	//var buf = TE.encode(a);
	let outBuf = new Buffer.alloc( buf.length );
	let o = b.step;
	b.step += buf.length;
	let keylen = b.key.length-5;
	b.step %= keylen;
	let _mask = 0x3F;
	let l = 0;
        //console.log( "Decode length:", buf.length );
	for( var n = 0; n < buf.length; n++ ) {
		let v = buf[n];
		let mask = _mask;

		if( (v & 0x80) == 0x00 )      { if( l ) throw new Error( "short utf8 sequence found" ); mask=0x3f; _mask = 0x3f; }
		else if( (v & 0xC0) == 0x80 ) { if( !l ) throw new Error( "invalid utf8 sequence" ); l--; _mask = 0x3f; }
		else if( (v & 0xE0) == 0xC0 ) { if( l ) throw new Error( "short utf8 sequence found" ); l = 1; mask=0x1;_mask = 0x3f; }  // 6 + 1 == 7
		else if( (v & 0xF0) == 0xE0 ) { if( l ) throw new Error( "short utf8 sequence found" ); l = 2; mask=0;  _mask = 0x1f; }  // 6 + 5 + 0 == 11 
		else if( (v & 0xF8) == 0xF0 ) { if( l ) throw new Error( "short utf8 sequence found" ); l = 3; mask=0;  _mask = 0x0f; }  // 6(2) + 4 + 0 == 16
		else if( (v & 0xFC) == 0xF8 ) { if( l ) throw new Error( "short utf8 sequence found" ); l = 4; mask=0;  _mask = 0x07; }  // 6(3) + 3 + 0 == 21
		else if( (v & 0xFE) == 0xFC ) { if( l ) throw new Error( "short utf8 sequence found" ); l = 5; mask=0;  _mask = 0x03; }  // 6(4) + 2 + 0 == 26

		if( mask )
			outBuf[n] = (v & ~mask ) | ( u8xor_code_encodings2[ ((v & mask)<<7) + (c[(n+o)%(keylen)]) ] & mask );
		else
			outBuf[n] = v;
	}
	return outBuf.toString( "utf8" );
}

function u8xor(a,b) {
	//var buf = Buffer.from(a, 'utf8');
	var buf = TE.encode(a);
	if( !b.keybuf ) { /*console.trace( "Key needs buf...." );*/ b.keybuf = TE.encode( b.key ); }
	let c = b.keybuf;

	var outBuf = new Uint8Array( buf.length );
	var o = b.step;
	b.step += buf.length;
	var keylen = b.key.length-5;
	b.step %= keylen;
	let _mask = 0x3F;
	let l = 0;
	
	for( let n = 0; n < buf.length; n++ ) {
		let v = buf[n];
		let mask = _mask;

		if( (v & 0x80) == 0x00 )      { if( l ) throw new Error( "short utf8 sequence found" ); mask=0x3f; _mask = 0x3f; }
		else if( (v & 0xC0) == 0x80 ) { if( !l ) throw new Error( "invalid utf8 sequence" ); l--; _mask = 0x3f; }
		else if( (v & 0xE0) == 0xC0 ) { if( l ) throw new Error( "short utf8 sequence found" ); l = 1; mask=0x1;_mask = 0x3f; }  // 6 + 1 == 7
		else if( (v & 0xF0) == 0xE0 ) { if( l ) throw new Error( "short utf8 sequence found" ); l = 2; mask=0;  _mask = 0x1f; }  // 6 + 5 + 0 == 11 
		else if( (v & 0xF8) == 0xF0 ) { if( l ) throw new Error( "short utf8 sequence found" ); l = 3; mask=0;  _mask = 0x0f; }  // 6(2) + 4 + 0 == 16
		else if( (v & 0xFC) == 0xF8 ) { if( l ) throw new Error( "short utf8 sequence found" ); l = 4; mask=0;  _mask = 0x07; }  // 6(3) + 3 + 0 == 21
		else if( (v & 0xFE) == 0xFC ) { if( l ) throw new Error( "short utf8 sequence found" ); l = 5; mask=0;  _mask = 0x03; }  // 6(4) + 2 + 0 == 26

		if( mask )
			outBuf[n] = (v & ~mask ) | ( u8xor_code_encodings2[ ((v & mask)<<7) + (c[(n+o)%(keylen)]) ] & mask );
		else
			outBuf[n] = v;
	}
	//console.log( "buf" , buf.toString('hex') );
	//console.log( "buf" , outBuf.toString('hex') );
	//return outBuf.toString( "utf8" );
	return TD.decode(outBuf);
}


SaltyRNG.u8xor = ("undefined" !== typeof Buffer)?u8xor_node:u8xor;

Object.freeze( SaltyRNG );

var TD;
var TE;

if( typeof TextDecoder === "undefined" ) {
	function myTextEncoder() {
		this.encode = function(s) {	
			var chars = [...s];
			var len = 0;
			for( var n = 0; n < chars.length; n++ ) {
				var chInt = chars[n].codePointAt(0);
				if( chInt < 128 ) 
					len++;
				else if( chInt < 0x800 ) 
					len += 2;
				else if( chInt < 0x10000 ) 
					len += 3;
				else if( chInt < 0x110000 ) 
					len += 4;
			}
			var out = new Uint8Array( len );
			len = 0;			
			for( var n = 0; n < chars.length; n++ ) {
				var chInt = chars[n].codePointAt(0);
				if( chInt < 128 ) 
					out[len++] = chInt;
				else if( chInt < 0x800 ) {
					out[len++] = ( (chInt & 0x7c0) >> 6 ) | 0xc0;
					out[len++] = ( (chInt & 0x03f) ) | 0x80;
				} else if( chInt < 0x10000 ) {
					out[len++] = ( (chInt & 0xf000) >> 12 ) | 0xE0;
					out[len++] = ( (chInt & 0x0fc0) >> 6 ) | 0x80;
					out[len++] = ( (chInt & 0x003f) ) | 0x80;
				} else if( chInt < 0x110000 ) {
					out[len++] = ( (chInt & 0x01c0000) >> 18 ) | 0xF0;
					out[len++] = ( (chInt & 0x003f000) >> 12 ) | 0xE0;
					out[len++] = ( (chInt & 0x0000fc0) >> 6 ) | 0x80;
					out[len++] = ( (chInt & 0x000003f) ) | 0x80;
				}
			}
			return out;
		};
	}
	function myTextDecoder() {
		this.decode = function(buf) {
			var out = '';
			for( var n = 0; n < buf.length; n++ ) {
				if( ( buf[n]& 0x80 ) == 0 )
					out += String.fromCodePoint( buf[n] );
				else if( ( buf[n] & 0xC0 ) == 0x80 ) ; else if( ( buf[n] & 0xE0 ) == 0xC0 ) {
					out += String.fromCodePoint( ( ( buf[n] & 0x1f ) << 6 ) | ( buf[n+1] & 0x3f ) );
					n++;
				} else if( ( buf[n] & 0xF0 ) == 0xE0 ) {
					out += String.fromCodePoint( ( ( buf[n] & 0xf ) << 12 ) | ( ( buf[n+1] & 0x3f ) << 6 ) | ( buf[n+2] & 0x3f ) );
					n+=2;
				} else if( ( buf[n] & 0xF8 ) == 0xF0 ) {
					out += String.fromCodePoint( ( ( buf[n] & 0x7 ) << 18 ) | ( ( buf[n+1] & 0x3f ) << 12 ) | ( ( buf[n+2] & 0x3f ) << 6 ) | ( buf[n+3] & 0x3f ) );
					n+=3;
				} else if( ( buf[n] & 0xFC ) == 0xF8 ) {
					out += String.fromCodePoint( ( ( buf[n] & 0x3 ) << 24 ) | ( ( buf[n+1] & 0x3f ) << 18 ) | ( ( buf[n+2] & 0x3f ) << 12 ) | ( ( buf[n+3] & 0x3f ) << 6 ) | ( buf[n+4] & 0x3f ) );
					n+=4;
				}
			}
			return out;
		};
	}
	TD = new myTextDecoder();
	TE = new myTextEncoder();
}
else {
	TD = new TextDecoder();
	TE = new TextEncoder();
}

export {GetCurrentTick};
export {SRG_XSWS_decryptData};
export {SRG_XSWS_decryptString};
export {SRG_XSWS_encryptData};
export {SRG_XSWS_encryptString};
export {SaltyRNG};
export {TickToTime};
export {base64ArrayBuffer};

