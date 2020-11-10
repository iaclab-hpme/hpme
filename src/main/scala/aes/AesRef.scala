// Chisel implementation of AES
// Function: AES model written by Scala
// Author: Yao Zhao

package hpme.aes

object AesLut {
    val subByteTable: Array[Int] = Array(
        0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
        0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
        0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
        0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
        0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
        0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
        0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
        0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
        0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
        0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
        0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
        0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
        0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
        0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
        0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
        0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16
    )

    val invSubByteTable: Array[Int] = Array(
        0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38, 0xbf, 0x40, 0xa3, 0x9e, 0x81, 0xf3, 0xd7, 0xfb,
        0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87, 0x34, 0x8e, 0x43, 0x44, 0xc4, 0xde, 0xe9, 0xcb,
        0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 0x3d, 0xee, 0x4c, 0x95, 0x0b, 0x42, 0xfa, 0xc3, 0x4e,
        0x08, 0x2e, 0xa1, 0x66, 0x28, 0xd9, 0x24, 0xb2, 0x76, 0x5b, 0xa2, 0x49, 0x6d, 0x8b, 0xd1, 0x25,
        0x72, 0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xd4, 0xa4, 0x5c, 0xcc, 0x5d, 0x65, 0xb6, 0x92,
        0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda, 0x5e, 0x15, 0x46, 0x57, 0xa7, 0x8d, 0x9d, 0x84,
        0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a, 0xf7, 0xe4, 0x58, 0x05, 0xb8, 0xb3, 0x45, 0x06,
        0xd0, 0x2c, 0x1e, 0x8f, 0xca, 0x3f, 0x0f, 0x02, 0xc1, 0xaf, 0xbd, 0x03, 0x01, 0x13, 0x8a, 0x6b,
        0x3a, 0x91, 0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea, 0x97, 0xf2, 0xcf, 0xce, 0xf0, 0xb4, 0xe6, 0x73,
        0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85, 0xe2, 0xf9, 0x37, 0xe8, 0x1c, 0x75, 0xdf, 0x6e,
        0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89, 0x6f, 0xb7, 0x62, 0x0e, 0xaa, 0x18, 0xbe, 0x1b,
        0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 0x79, 0x20, 0x9a, 0xdb, 0xc0, 0xfe, 0x78, 0xcd, 0x5a, 0xf4,
        0x1f, 0xdd, 0xa8, 0x33, 0x88, 0x07, 0xc7, 0x31, 0xb1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xec, 0x5f,
        0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d, 0x2d, 0xe5, 0x7a, 0x9f, 0x93, 0xc9, 0x9c, 0xef,
        0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0, 0xc8, 0xeb, 0xbb, 0x3c, 0x83, 0x53, 0x99, 0x61,
        0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 0x26, 0xe1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0c, 0x7d
    )
}

class AesRef (Nk: Int = 4) {

    def Nb = 4
    def Nr = Nk + 6

    val key = new Array[Byte](Nk*4)
    val expandedKeys = new Array[Array[Byte]](Nr+1)
    var debugMode = false

    def setDebugMode(mode: Boolean): Unit = debugMode = mode

    def setKey(keyIn: Array[Byte]): Unit = {
        keyIn.copyToArray(key)
        keyExpansion(key).copyToArray(expandedKeys)
        if (debugMode){
            println("Expanded keys:")
            printExpandedKeys(expandedKeys)
        }
    }

    def addRoundKey(state: Array[Byte], round: Int): Array[Byte] = {
        val stateAddKey = new Array[Byte](Nb*4)
        var i = 0
        for (i <- 0 until Nb*4) {
            stateAddKey(i) = (state(i) ^ expandedKeys(round)(i)).toByte
        }
        stateAddKey
    }

    def x01(b: Byte): Byte = b
    def x02(b: Byte): Byte = if (b < 0) ((b<<1) ^ 0x1b).toByte else ((b<<1) & 0xff).toByte
    def x03(b: Byte): Byte = (x02(b) ^ b).toByte
    def x09(b: Byte): Byte = (x02(x02(x02(b))) ^ b).toByte
    def x0b(b: Byte): Byte = (x02(x02(x02(b))) ^ x02(b) ^ b).toByte
    def x0d(b: Byte): Byte = (x02(x02(x02(b))) ^ x02(x02(b)) ^ b).toByte
    def x0e(b: Byte): Byte = (x02(x02(x02(b))) ^ x02(x02(b)) ^ x02(b)).toByte

    def mixColumns(state: Array[Byte]): Array[Byte] = {
        val temp = new Array[Byte](Nb*4)
        var i = 0
        for (i <- 0 until Nb) {
            temp(i*4  ) = (x02(state(i*4)) ^ x03(state(i*4+1)) ^ x01(state(i*4+2)) ^ x01(state(i*4+3))).toByte
            temp(i*4+1) = (x01(state(i*4)) ^ x02(state(i*4+1)) ^ x03(state(i*4+2)) ^ x01(state(i*4+3))).toByte
            temp(i*4+2) = (x01(state(i*4)) ^ x01(state(i*4+1)) ^ x02(state(i*4+2)) ^ x03(state(i*4+3))).toByte
            temp(i*4+3) = (x03(state(i*4)) ^ x01(state(i*4+1)) ^ x01(state(i*4+2)) ^ x02(state(i*4+3))).toByte
        }
        temp
    }

    def invMixColumns(state: Array[Byte]): Array[Byte] = {
        val temp = new Array[Byte](Nb*4)
        var i = 0
        for (i <- 0 until Nb) {
            temp(i*4  ) = (x0e(state(i*4)) ^ x0b(state(i*4+1)) ^ x0d(state(i*4+2)) ^ x09(state(i*4+3))).toByte
            temp(i*4+1) = (x09(state(i*4)) ^ x0e(state(i*4+1)) ^ x0b(state(i*4+2)) ^ x0d(state(i*4+3))).toByte
            temp(i*4+2) = (x0d(state(i*4)) ^ x09(state(i*4+1)) ^ x0e(state(i*4+2)) ^ x0b(state(i*4+3))).toByte
            temp(i*4+3) = (x0b(state(i*4)) ^ x0d(state(i*4+1)) ^ x09(state(i*4+2)) ^ x0e(state(i*4+3))).toByte
        }
        temp
    }

    def subByte(x: Byte): Byte = AesLut.subByteTable(x&0xff).toByte
    def invSubByte(x: Byte): Byte = AesLut.invSubByteTable(x&0xff).toByte

    def subBytes(state: Array[Byte]): Array[Byte] = state.map(subByte(_))
    def invSubBytes(state: Array[Byte]): Array[Byte] = state.map(invSubByte(_))

    def shiftRows(state: Array[Byte]): Array[Byte] = {
        Array(state(0), state(5), state(10), state(15),
            state(4), state(9), state(14), state(3),
            state(8), state(13), state(2), state(7),
            state(12), state(1), state(6), state(11))
    }

    def invShiftRows(state: Array[Byte]): Array[Byte] = {
        Array(state(0), state(13), state(10), state(7),
            state(4), state(1), state(14), state(11),
            state(8), state(5), state(2), state(15),
            state(12), state(9), state(6), state(3))
    }

    def printHexByte(hexByte: Byte): Unit = {if ((hexByte&0xf0)==0x00) printf("0%1x", hexByte) else printf("%2x", hexByte)}

    def printState(state: Array[Byte], title: String): Unit = {printf(s"$title:"); state.foreach(printHexByte); printf("\n")}

    def printExpandedKeys(expandedKeys: Array[Array[Byte]]): Unit = expandedKeys.foreach(x => {x.foreach(printHexByte); printf("\n")})

    def keyExpansion(key: Array[Byte]):Array[Array[Byte]] = {
        val rconInt: Array[Int] = Array(0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36)
        val rcon: Array[Byte] = rconInt.map(_.toByte)
        val roundKeyExpansion = new Array[Byte](4*Nb*(Nr+1))
        var i, j = 0
        for (i <- 0 until Nk; j <- 0 until 4) {
            roundKeyExpansion(4*i+j) = key(4*i+j)
        }
        for (i <- Nk until Nb*(Nr+1)) {
            var tempWord = new Array[Byte](4)
            for (j <- 0 until 4) {
                tempWord(j) = roundKeyExpansion(4*i-4+j)
            }
            if (i%Nk == 0) {
                tempWord = Array(tempWord(1), tempWord(2), tempWord(3), tempWord(0))
                tempWord = tempWord.map(subByte(_))
                tempWord(0) = (tempWord(0) ^ rcon(i/Nk - 1)).toByte
            }
            else if ((Nk==8) && (i%Nk == 4)) {
                tempWord = tempWord.map(subByte(_))
            }
            for (j <- 0 until 4) {
                roundKeyExpansion(4*i+j) = (roundKeyExpansion(4*(i-Nk)+j) ^ tempWord(j)).toByte
            }
        }

        val tempMatrix = new Array[Array[Byte]](Nr+1)
        val tempRow = new Array[Byte](4*Nb)
        var k = 0
        for (k <- 0 until (Nr+1)) {
            tempMatrix(k) = new Array[Byte](4*Nb)
            for (i <- 0 until Nb; j <- 0 until 4) {
                tempMatrix(k)(4*i+j) = roundKeyExpansion(k*(4*Nb) + i*4 + j)
            }
        }

        tempMatrix
    }

    def cipher(plain: Array[Byte]): Array[Byte] = {
        var state: Array[Byte] = new Array[Byte](4*Nb)

        state = plain //init
        if (debugMode) printState(state, "Input     ")
        state = addRoundKey(state, 0) //round 0
        if (debugMode) printState(state, "AddRndKey ")

        var i = 0
        for (i <- 1 to Nr) {
            state = subBytes(state)
            if (debugMode) printState(state, "SubBytes  ")
            state = shiftRows(state)
            if (debugMode) printState(state, "ShiftRows ")
            if (i < Nr) {
                state = mixColumns(state)
                if (debugMode) printState(state, "MixColumns")
            }
            state = addRoundKey(state, i)
            if (debugMode) printState(state, "AddRndKey ")
        }

        state
    }

    def invCipher(cipher: Array[Byte]): Array[Byte] = {
        var state: Array [Byte] = new Array[Byte](4*Nb)

        state = cipher
        if (debugMode) printState(state, "IInput     ")
        state = addRoundKey(state, Nr) //round Nr
        if (debugMode) printState(state, "IAddRndKey ")

        var i = 0
        for (i <- (Nr-1) to 0 by -1) {
            state = invShiftRows(state)
            if (debugMode) printState(state, "IShiftRows ")
            state = invSubBytes(state)
            if (debugMode) printState(state, "ISubBytes  ")
            state = addRoundKey(state, i)
            if (debugMode) printState(state, "IAddRndKey ")
            if (i > 0) {
                state = invMixColumns(state)
                if (debugMode) printState(state, "IMixColumns")
            }
        }

        state
    }

}

object AesRefTest extends App {
    //val aesref = new AesRef(4)
    val aesref = new AesRef(8)

    val plainInt = Array(0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff)
    val plain: Array[Byte] = plainInt.map(_.toByte)
    //aesref.printState(plain, "Input    ")
    //val subBytesState = aesref.subBytes(plain)
    //aesref.printState(subBytesState, "SubBytes ")
    //val shiftRowsState = aesref.shiftRows(subBytesState)
    //aesref.printState(shiftRowsState, "ShiftRows")

    //val keyInt = Array(0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f, 0x3c)
    //val key: Array[Byte] = keyInt.map(_.toByte)
    //val expandedKeys = aesref.keyExpansion(key)
    //aesref.printExpandedKeys(expandedKeys)
    //aesref.setKey(key)

    //val key2Int = Array(0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17)
    val key2Int = (0 to 31).toArray
    val key2: Array[Byte] = key2Int.map(_.toByte)
    aesref.setKey(key2)
    aesref.printState(plain, "Plain")
    val cipher: Array[Byte] = aesref.cipher(plain)
    aesref.printState(cipher, "Cipher")
    val plain2: Array[Byte] = aesref.invCipher(cipher)
    aesref.printState(plain2, "InvCipher")
}