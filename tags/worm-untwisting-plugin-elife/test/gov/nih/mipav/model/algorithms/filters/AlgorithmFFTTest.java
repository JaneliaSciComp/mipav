package gov.nih.mipav.model.algorithms.filters;


public class AlgorithmFFTTest {
//    private void selfTest() {
//        float a[] = null;
//        float b[] = null;
//        float c[] = null;
//        float d[] = null;
//        float newA[] = null;
//        float newC[] = null;
//        int FORWARD_INVERSE = 1;
//        int SPATIAL_SHIFT = 2;
//        int FREQUENCY_SHIFT = 3;
//        int x, y, z;
//        int nDims = 2;
//        int extents[] = null;
//        int sliceSize = 0;
//        int newExtents[] = null;
//        int nTests = 148;
//        int i, j;
//        int arrayLength = 0;
//        int imageType = ModelStorageBase.FLOAT;
//        ModelImage forwardImage = null;
//        boolean createNewImage = true;
//        ModelImage resultImage = null;
//        ModelImage inverseImage = null;
//        int transformDir;
//        boolean logMagDisplay = true;
//        boolean unequalDim = true;
//        ;
//        boolean image25D = false;
//        boolean imageCrop = false;
//        int kernelDiameter = 15;
//        int butterworthOrder = 1;
//        float freq1 = 0.4f;
//        float freq2 = 0.7f;
//        AlgorithmFFT FFTAlgo;
//        double error[];
//        boolean doCrop = false;
//        int dimTest;
//        int newLength;
//        int start[] = null;
//        int end[] = null;
//        int newArrayLength;
//        int u;
//        int v;
//        int w;
//        int testType;
//        double s = 0.5;
//        int index;
//        double realF;
//        double arg;
//        double cosuv;
//        double sinuv;
//        double cosxy;
//        double sinxy;
//        RandomNumberGen randomGen = new RandomNumberGen();
//        ViewUserInterface UI = ViewUserInterface.getReference();
//        boolean foundError[] = new boolean[nTests];
//        int errorsFound = 0;
//
//        // The first 120 perform a forward transform, then the inverse, and
//        // verify that the result is
//        // the same as the original data
//        // The first 60 tests create a destImage
//        // Tests 60 thru 119 only use the srcImage
//        // Tests 120 thru 131 verify the spatial translation property
//        // The spatial translation property:
//        // F(u,v)*exp(j*2*PI*(u*x0/M + v*y0/N)) <=> f(x - x0, y - y0)
//        // Let x0 = sM, y0 = sN
//        // F(u,v)*exp(j*2*PI*s*(u + v)) <=> f(x - sM, y - sN)
//        // Re(F(u,v))*cos(2*PI*s*(u+v)) - Im(F(u,v))*sin(2*PI*s*(u+v))
//        // +j*[Re(F(u,v))*sin(2*PI*s(u+v)) + Im(F(u,v))*cos(2*PI*s*(u+v)) <=>
//        // f(x - sM, y - sN)
//        // For the special case of s = 0.5:
//        // f(x - extents[0]/2, y - extents[1]/2) <=> F(u, v) * ((-1)**(u + v))
//        // Tests 132 thru 135 verify the frequency translation property
//        // f(x,y) * ((-1)**(x+y)) <=> F(u - extents[0]/2, v - extents[1]/2)
//        createNewImage = true;
//        testType = FORWARD_INVERSE;
//        for (i = 0; i < 136; i++) {
//            if (i == 60) {
//                createNewImage = false;
//            }
//            UI.setDataText("Running test = " + i + "\n");
//            if ((i == 0) || (i == 60)) {
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 256;
//                extents[1] = 256;
//                kernelDiameter = 15;
//                imageCrop = true;
//            } // if ((i == 0) || ( i == 60))
//            else if ((i == 1) || (i == 61)) {
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 256;
//                extents[1] = 256;
//                kernelDiameter = 15;
//                imageCrop = false;
//            } else if ((i == 2) || (i == 62)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                kernelDiameter = 15;
//                image25D = false;
//                imageCrop = true;
//            } else if ((i == 3) || (i == 63)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                kernelDiameter = 15;
//                image25D = false;
//                imageCrop = false;
//            } else if ((i == 4) || (i == 64)) {
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 256;
//                extents[1] = 256;
//            } else if ((i == 5) || (i == 65)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                image25D = false;
//            } else if ((i == 6) || (i == 66)) {
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 256;
//                extents[1] = 256;
//                butterworthOrder = 1;
//            } else if ((i == 7) || (i == 67)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                butterworthOrder = 1;
//                image25D = false;
//            } else if ((i == 8) || (i == 68)) {
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 256;
//                kernelDiameter = 15;
//                unequalDim = true;
//                imageCrop = true;
//            } // else if ((i == 8) || (i == 68))
//            else if ((i == 9) || (i == 69)) {
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 256;
//                kernelDiameter = 15;
//                unequalDim = true;
//                imageCrop = false;
//            } else if ((i == 10) || (i == 70)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 64;
//                extents[1] = 128;
//                extents[2] = 256;
//                kernelDiameter = 15;
//                unequalDim = true;
//                image25D = false;
//                imageCrop = true;
//            } else if ((i == 11) || (i == 71)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 64;
//                extents[1] = 128;
//                extents[2] = 256;
//                kernelDiameter = 15;
//                unequalDim = true;
//                image25D = false;
//                imageCrop = false;
//            } else if ((i == 12) || (i == 72)) {
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 256;
//                unequalDim = true;
//            } else if ((i == 13) || (i == 73)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 64;
//                extents[1] = 128;
//                extents[2] = 256;
//                unequalDim = true;
//                image25D = false;
//            } else if ((i == 14) || (i == 74)) {
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 256;
//                butterworthOrder = 1;
//                unequalDim = true;
//            } else if ((i == 15) || (i == 75)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 64;
//                extents[1] = 128;
//                extents[2] = 256;
//                butterworthOrder = 1;
//                unequalDim = true;
//                image25D = false;
//            } else if ((i == 16) || (i == 76)) {
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 256;
//                kernelDiameter = 15;
//                unequalDim = false;
//                imageCrop = true;
//            } // if ((i == 16) || (i == 76))
//            else if ((i == 17) || (i == 77)) {
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 256;
//                kernelDiameter = 15;
//                unequalDim = false;
//                imageCrop = false;
//            } else if ((i == 18) || (i == 78)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 64;
//                extents[1] = 128;
//                extents[2] = 256;
//                kernelDiameter = 15;
//                unequalDim = false;
//                image25D = false;
//                imageCrop = true;
//            } else if ((i == 19) || (i == 79)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 32;
//                extents[1] = 64;
//                extents[2] = 128;
//                kernelDiameter = 15;
//                unequalDim = false;
//                image25D = false;
//                imageCrop = false;
//            } else if ((i == 20) || (i == 80)) {
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 256;
//                unequalDim = false;
//            } else if ((i == 21) || (i == 81)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 64;
//                extents[1] = 128;
//                extents[2] = 256;
//                unequalDim = false;
//                image25D = false;
//            } else if ((i == 22) || (i == 82)) {
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 256;
//                butterworthOrder = 1;
//                unequalDim = false;
//            } else if ((i == 23) || (i == 83)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 64;
//                extents[1] = 128;
//                extents[2] = 256;
//                butterworthOrder = 1;
//                unequalDim = false;
//                image25D = false;
//            } else if ((i == 24) || (i == 84)) {
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 111;
//                extents[1] = 239;
//                kernelDiameter = 15;
//                unequalDim = true;
//                imageCrop = true;
//            } // else if ((i == 24) || (i == 84))
//            else if ((i == 25) || (i == 85)) {
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 111;
//                extents[1] = 239;
//                kernelDiameter = 15;
//                unequalDim = true;
//                imageCrop = false;
//            } else if ((i == 26) || (i == 86)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 59;
//                extents[1] = 111;
//                extents[2] = 239;
//                kernelDiameter = 15;
//                unequalDim = true;
//                image25D = false;
//                imageCrop = true;
//            } else if ((i == 27) || (i == 87)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 59;
//                extents[1] = 111;
//                extents[2] = 239;
//                kernelDiameter = 15;
//                unequalDim = true;
//                image25D = false;
//                imageCrop = false;
//            } else if ((i == 28) || (i == 88)) {
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 111;
//                extents[1] = 239;
//                unequalDim = true;
//            } else if ((i == 29) || (i == 89)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 59;
//                extents[1] = 111;
//                extents[2] = 239;
//                unequalDim = true;
//                image25D = false;
//            } else if ((i == 30) || (i == 90)) {
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 111;
//                extents[1] = 239;
//                butterworthOrder = 1;
//                unequalDim = true;
//            } else if ((i == 31) || (i == 91)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 59;
//                extents[1] = 111;
//                extents[2] = 239;
//                butterworthOrder = 1;
//                unequalDim = true;
//                image25D = false;
//            } else if ((i == 32) || (i == 92)) {
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 111;
//                extents[1] = 239;
//                kernelDiameter = 15;
//                unequalDim = false;
//                imageCrop = true;
//            } // else if ((i == 32) || (i == 92))
//            else if ((i == 33) || (i == 93)) {
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 111;
//                extents[1] = 239;
//                kernelDiameter = 15;
//                unequalDim = false;
//                imageCrop = false;
//            } else if ((i == 34) || (i == 94)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 59;
//                extents[1] = 111;
//                extents[2] = 239;
//                kernelDiameter = 15;
//                unequalDim = false;
//                image25D = false;
//                imageCrop = true;
//            } else if ((i == 35) || (i == 95)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 59;
//                extents[1] = 111;
//                extents[2] = 239;
//                kernelDiameter = 15;
//                unequalDim = false;
//                image25D = false;
//                imageCrop = false;
//            } else if ((i == 36) || (i == 96)) {
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 111;
//                extents[1] = 239;
//                unequalDim = false;
//            } else if ((i == 37) || (i == 97)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 59;
//                extents[1] = 111;
//                extents[2] = 239;
//                unequalDim = false;
//                image25D = false;
//            } else if ((i == 38) || (i == 98)) {
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 111;
//                extents[1] = 239;
//                butterworthOrder = 1;
//                unequalDim = false;
//            } else if ((i == 39) || (i == 99)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 59;
//                extents[1] = 111;
//                extents[2] = 239;
//                butterworthOrder = 1;
//                unequalDim = false;
//                image25D = false;
//            } else if ((i == 40) || (i == 100)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                kernelDiameter = 15;
//                image25D = true;
//                imageCrop = true;
//            } else if ((i == 41) || (i == 101)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                kernelDiameter = 15;
//                image25D = true;
//                imageCrop = false;
//            } else if ((i == 42) || (i == 102)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                image25D = true;
//            } else if ((i == 43) || (i == 103)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                butterworthOrder = 1;
//                image25D = true;
//            } else if ((i == 44) || (i == 104)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 64;
//                extents[1] = 128;
//                extents[2] = 256;
//                kernelDiameter = 15;
//                unequalDim = true;
//                image25D = true;
//                imageCrop = true;
//            } else if ((i == 45) || (i == 105)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 64;
//                extents[1] = 128;
//                extents[2] = 256;
//                kernelDiameter = 15;
//                unequalDim = true;
//                image25D = true;
//                imageCrop = false;
//            } else if ((i == 46) || (i == 106)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 64;
//                extents[1] = 128;
//                extents[2] = 256;
//                unequalDim = true;
//                image25D = true;
//            } else if ((i == 47) || (i == 107)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 64;
//                extents[1] = 128;
//                extents[2] = 256;
//                butterworthOrder = 1;
//                unequalDim = true;
//                image25D = true;
//            } else if ((i == 48) || (i == 108)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 64;
//                extents[1] = 128;
//                extents[2] = 256;
//                kernelDiameter = 15;
//                unequalDim = false;
//                image25D = true;
//                imageCrop = true;
//            } else if ((i == 49) || (i == 109)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 64;
//                extents[1] = 128;
//                extents[2] = 256;
//                kernelDiameter = 15;
//                unequalDim = false;
//                image25D = true;
//                imageCrop = false;
//            } else if ((i == 50) || (i == 110)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 64;
//                extents[1] = 128;
//                extents[2] = 256;
//                unequalDim = false;
//                image25D = true;
//            } else if ((i == 51) || (i == 111)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 64;
//                extents[1] = 128;
//                extents[2] = 256;
//                butterworthOrder = 1;
//                unequalDim = false;
//                image25D = true;
//            } else if ((i == 52) || (i == 112)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 59;
//                extents[1] = 111;
//                extents[2] = 239;
//                kernelDiameter = 15;
//                unequalDim = true;
//                image25D = true;
//                imageCrop = true;
//            } else if ((i == 53) || (i == 113)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 59;
//                extents[1] = 111;
//                extents[2] = 239;
//                kernelDiameter = 15;
//                unequalDim = true;
//                image25D = true;
//                imageCrop = false;
//            } else if ((i == 54) || (i == 114)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 59;
//                extents[1] = 111;
//                extents[2] = 239;
//                unequalDim = true;
//                image25D = true;
//            } else if ((i == 55) || (i == 115)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 59;
//                extents[1] = 111;
//                extents[2] = 239;
//                butterworthOrder = 1;
//                unequalDim = true;
//                image25D = true;
//            } else if ((i == 56) || (i == 116)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 59;
//                extents[1] = 111;
//                extents[2] = 239;
//                kernelDiameter = 15;
//                unequalDim = false;
//                image25D = true;
//                imageCrop = true;
//            } else if ((i == 57) || (i == 117)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 59;
//                extents[1] = 111;
//                extents[2] = 239;
//                kernelDiameter = 15;
//                unequalDim = false;
//                image25D = true;
//                imageCrop = false;
//            } else if ((i == 58) || (i == 118)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 59;
//                extents[1] = 111;
//                extents[2] = 239;
//                unequalDim = false;
//                image25D = true;
//            } else if ((i == 59) || (i == 119)) {
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 59;
//                extents[1] = 111;
//                extents[2] = 239;
//                butterworthOrder = 1;
//                unequalDim = false;
//                image25D = true;
//            } else if (i == 120) {
//                testType = SPATIAL_SHIFT;
//                s = 0.25;
//                createNewImage = true;
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 256;
//                extents[1] = 256;
//            } else if (i == 121) {
//                testType = SPATIAL_SHIFT;
//                s = 0.25;
//                createNewImage = false;
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 256;
//                extents[1] = 256;
//            } else if (i == 122) {
//                testType = SPATIAL_SHIFT;
//                s = 0.25;
//                createNewImage = true;
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                image25D = false;
//            } else if (i == 123) {
//                testType = SPATIAL_SHIFT;
//                s = 0.25;
//                createNewImage = false;
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                image25D = false;
//            } else if (i == 124) {
//                testType = SPATIAL_SHIFT;
//                s = 0.5;
//                createNewImage = true;
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 256;
//                extents[1] = 256;
//            } else if (i == 125) {
//                testType = SPATIAL_SHIFT;
//                s = 0.5;
//                createNewImage = false;
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 256;
//                extents[1] = 256;
//            } else if (i == 126) {
//                testType = SPATIAL_SHIFT;
//                s = 0.5;
//                createNewImage = true;
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                image25D = false;
//            } else if (i == 127) {
//                testType = SPATIAL_SHIFT;
//                s = 0.5;
//                createNewImage = false;
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                image25D = false;
//            } else if (i == 128) {
//                testType = SPATIAL_SHIFT;
//                s = 0.75;
//                createNewImage = true;
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 256;
//                extents[1] = 256;
//            } else if (i == 129) {
//                testType = SPATIAL_SHIFT;
//                s = 0.75;
//                createNewImage = false;
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 256;
//                extents[1] = 256;
//            } else if (i == 130) {
//                testType = SPATIAL_SHIFT;
//                s = 0.75;
//                createNewImage = true;
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                image25D = false;
//            } else if (i == 131) {
//                testType = SPATIAL_SHIFT;
//                s = 0.75;
//                createNewImage = false;
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                image25D = false;
//            } else if (i == 132) {
//                testType = FREQUENCY_SHIFT;
//                createNewImage = true;
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 256;
//                extents[1] = 256;
//            } else if (i == 133) {
//                testType = FREQUENCY_SHIFT;
//                createNewImage = false;
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 256;
//                extents[1] = 256;
//            } else if (i == 134) {
//                testType = FREQUENCY_SHIFT;
//                createNewImage = true;
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                image25D = false;
//            } else if (i == 135) {
//                testType = FREQUENCY_SHIFT;
//                createNewImage = false;
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                image25D = false;
//            }
//            if (nDims == 3) {
//                sliceSize = extents[0] * extents[1];
//            }
//
//            doCrop = false;
//            if (imageCrop && (constructionMethod == WINDOW)) {
//                newExtents = new int[nDims];
//
//                // If imageCrop is false:
//                // Find the lowest power of 2 number not less than kdim +
//                // (dimLengths[i]) - 1.
//                // If imageCrop is true:
//                // Find the lowest power of 2 number not less than
//                // (dimLengths[i])
//                // This must be done to prevent aliasing in using a frequency
//                // filter,
//                // and to have a power of 2 for the FFT.
//                // Make all dimensions equal to this the largest of the above
//                // dimensions if
//                // unequalDim is false
//
//                arrayLength = 1;
//
//                for (j = 0; j < nDims; j++) {
//                    arrayLength *= extents[j];
//                    newExtents[j] = extents[j];
//                }
//
//                if ((nDims == 3) && (image25D)) {
//
//                    for (j = 0; j < 2; j++) {
//
//                        for (dimTest = newExtents[j]; dimTest > 1; dimTest = dimTest / 2) {
//
//                            if ((dimTest % 2) != 0) {
//                                dimTest = 1;
//
//                                // log2x = logex/loge2
//                                newExtents[j] = 1 + (int) (Math
//                                        .log((double) newExtents[j]) / Math
//                                        .log(2.0));
//                                newExtents[j] = Math.round((float) (Math.pow(
//                                        2.0, (double) newExtents[j])));
//                            }
//                        }
//                    } // for (j = 0; j < 2; j++)
//                } // if (nDIms == 3) && (image25D))
//                else { // not image25D
//
//                    for (j = 0; j < nDims; j++) {
//
//                        for (dimTest = newExtents[j]; dimTest > 1; dimTest = dimTest / 2) {
//
//                            if ((dimTest % 2) != 0) {
//                                dimTest = 1;
//
//                                // log2x = logex/loge2
//                                newExtents[j] = 1 + (int) (Math
//                                        .log((double) newExtents[j]) / Math
//                                        .log(2.0));
//                                newExtents[j] = Math.round((float) (Math.pow(
//                                        2.0, (double) newExtents[j])));
//                            }
//                        }
//                    } // for (j = 0; j < nDims; j++)
//                } // else not image25D
//
//                if (!unequalDim) { // if dimensions are to be made equal
//                    newLength = 0;
//
//                    if (image25D) {
//
//                        for (j = 0; j < 2; j++) {
//
//                            if (newExtents[j] > newLength) {
//                                newLength = newExtents[j];
//                            }
//                        }
//
//                        for (j = 0; j < 2; j++) {
//                            newExtents[j] = newLength;
//                        }
//                    } else { // not image25D
//
//                        for (j = 0; j < nDims; j++) {
//
//                            if (newExtents[j] > newLength) {
//                                newLength = newExtents[j];
//                            }
//                        }
//
//                        for (j = 0; j < nDims; j++) {
//                            newExtents[j] = newLength;
//                        }
//                    } // else not image25D
//                } // end of if (!unequalDim)
//
//                if (image25D) {
//
//                    for (j = 0; j < 2; j++) {
//
//                        if ((extents[j] + kernelDiameter - 1) > newExtents[j]) {
//                            doCrop = true;
//                        }
//                    }
//                } // if (image25D)
//                else { // not image25D
//
//                    for (j = 0; j < nDims; j++) {
//
//                        if ((extents[j] + kernelDiameter - 1) > newExtents[j]) {
//                            doCrop = true;
//                        }
//                    }
//                } // else not image25D
//
//                if (doCrop) {
//                    start = new int[nDims];
//                    end = new int[nDims];
//
//                    if ((nDims == 3) && (image25D)) {
//
//                        for (j = 0; j < 2; j++) {
//
//                            if ((extents[j] + kernelDiameter - 1) > newExtents[j]) {
//                                start[j] = ((extents[j] + kernelDiameter - 1 - newExtents[j]) / 2)
//                                        + ((extents[j] + kernelDiameter - 1 - newExtents[j]) % 2);
//                                end[j] = extents[j]
//                                        - 1
//                                        - ((extents[j] + kernelDiameter - 1 - newExtents[j]) / 2);
//                            } else {
//                                start[j] = 0;
//                                end[j] = extents[j] - 1;
//                            }
//                        }
//
//                        start[2] = 0;
//                        end[2] = extents[2] - 1;
//                    } // if ((nDims == 3) && (image25D))
//                    else { // not image25D
//
//                        for (j = 0; j < nDims; j++) {
//
//                            if ((extents[j] + kernelDiameter - 1) > newExtents[j]) {
//                                start[j] = ((extents[j] + kernelDiameter - 1 - newExtents[j]) / 2)
//                                        + ((extents[j] + kernelDiameter - 1 - newExtents[j]) % 2);
//                                end[j] = extents[j]
//                                        - 1
//                                        - ((extents[j] + kernelDiameter - 1 - newExtents[j]) / 2);
//                            } else {
//                                start[j] = 0;
//                                end[j] = extents[j] - 1;
//                            }
//                        }
//                    } // else not image25D
//                } // if (doCrop)
//            } // if (imageCrop && (constructionMethod == WINDOW))
//
//            arrayLength = extents[0];
//            for (j = 1; j < nDims; j++) {
//                arrayLength *= extents[j];
//            }
//
//            a = new float[arrayLength];
//            c = new float[arrayLength];
//
//            for (j = 0; j < arrayLength; j++) {
//                c[j] = randomGen.genUniformRandomNum(0.0f, 1.0f);
//                a[j] = c[j];
//            } // for (j = 0; j < arrayLength; j++)
//
//            forwardImage = new ModelImage(imageType, extents, "forwardImage");
//
//            try {
//                forwardImage.importData(0, a, true);
//            } catch (IOException e) {
//                displayError("IOException on forwardImage.importData(0, a, true)");
//            }
//
//            forwardImage.setOriginalCropCheckbox(imageCrop);
//
//            String name = forwardImage.getImageName() + "_FFT";
//            transformDir = FORWARD;
//
//            if (createNewImage) {
//
//                try {
//                    resultImage = (ModelImage) forwardImage.clone();
//                    resultImage.setImageName(name);
//                    resultImage.resetVOIs();
//
//                    // Make algorithm
//                    FFTAlgo = new AlgorithmFFT(resultImage, forwardImage,
//                            transformDir, logMagDisplay, unequalDim, image25D,
//                            imageCrop, kernelDiameter, filterType, freq1,
//                            freq2, constructionMethod, butterworthOrder);
//                    FFTAlgo.calcStoreInDest();
//
//                } catch (OutOfMemoryError e) {
//                    displayError("AlgorithmFFT: unable to allocate enough memory");
//
//                    if (resultImage != null) {
//                        resultImage.disposeLocal(); // Clean up memory of result
//                                                    // image
//                        resultImage = null;
//                    }
//
//                    return;
//                }
//            } else {
//
//                try {
//
//                    // No need to make new image space because the user has
//                    // choosen to replace the source image
//                    // Make the algorithm class
//                    FFTAlgo = new AlgorithmFFT(forwardImage, transformDir,
//                            logMagDisplay, unequalDim, image25D, imageCrop,
//                            kernelDiameter, filterType, freq1, freq2,
//                            constructionMethod, butterworthOrder);
//
//                    FFTAlgo.calcInPlace();
//                } catch (OutOfMemoryError e) {
//                    displayError("AlgorithmFFT: unable to allocate enough memory");
//
//                    return;
//                }
//            }
//
//            FFTAlgo.finalize();
//            if (testType == SPATIAL_SHIFT) {
//                // The spatial translation property:
//                // F(u,v)*exp(j*2*PI*(u*x0/M + v*y0/N)) <=> f(x - x0, y - y0)
//                // Let x0 = sM, y0 = sN
//                // F(u,v)*exp(j*2*PI*s*(u + v)) <=> f(x - sM, y - sN)
//                // Re(F(u,v))*cos(2*PI*s*(u+v)) - Im(F(u,v))*sin(2*PI*s*(u+v))
//                // +j*[Re(F(u,v))*sin(2*PI*s(u+v)) +
//                // Im(F(u,v))*cos(2*PI*s*(u+v)) <=> f(x - sM, y - sN)
//                // For the special case of s = 0.5:
//                // f(x - extents[0]/2, y - extents[1]/2) <=> F(u, v) * ((-1)**(u
//                // + v))
//                b = new float[arrayLength];
//                d = new float[arrayLength];
//                newC = new float[arrayLength];
//                if (createNewImage) {
//                    try {
//                        resultImage.exportComplexData(0, arrayLength, a, b);
//                    } catch (IOException e) {
//                        displayError("IOException error on resultImage.exportComplexData(0, arrayLength, a, b)");
//                        return;
//                    }
//                } // if (createNewImage)
//                else { // not createNewImage
//                    try {
//                        forwardImage.exportComplexData(0, arrayLength, a, b);
//                    } catch (IOException e) {
//                        displayError("IOException error on forwardImage.exportComplexData(0, arrayLength, a, b)");
//                        return;
//                    }
//                } // else not createNewImage
//                testCenter(a, b, extents);
//                if (nDims == 2) {
//                    for (v = 0; v < extents[1]; v++) {
//                        for (u = 0; u < extents[0]; u++) {
//                            arg = 2.0 * Math.PI * s * (u + v);
//                            cosuv = Math.cos(arg);
//                            sinuv = Math.sin(arg);
//                            index = u + v * extents[0];
//                            realF = a[index] * cosuv - b[index] * sinuv;
//                            b[index] = (float) (a[index] * sinuv + b[index]
//                                    * cosuv);
//                            a[index] = (float) realF;
//                        }
//                    }
//                } else {
//                    for (w = 0; w < extents[2]; w++) {
//                        for (v = 0; v < extents[1]; v++) {
//                            for (u = 0; u < extents[0]; u++) {
//                                arg = 2.0 * Math.PI * s * (u + v + w);
//                                cosuv = Math.cos(arg);
//                                sinuv = Math.sin(arg);
//                                index = u + v * extents[0] + w * sliceSize;
//                                realF = a[index] * cosuv - b[index] * sinuv;
//                                b[index] = (float) (a[index] * sinuv + b[index]
//                                        * cosuv);
//                                a[index] = (float) realF;
//                            }
//                        }
//                    }
//                }
//                testCenter(a, b, extents);
//
//                if (createNewImage) {
//                    try {
//                        resultImage.importComplexData(0, a, b, true, true);
//                    } catch (IOException e) {
//                        displayError("IOException error on resultImage.importComplexData(0, a, b, true, true)");
//                        return;
//                    }
//                } // if (createNewImage)
//                else { // not createNewImage
//                    try {
//                        forwardImage.importComplexData(0, a, b, true, true);
//                    } catch (IOException e) {
//                        displayError("IOException error on forwardImage.importComplexData(0, a, b, true, true)");
//                        return;
//                    }
//                } // else not createNewImage
//                transformDir = INVERSE;
//
//                if (createNewImage) {
//
//                    try {
//                        inverseImage = (ModelImage) resultImage.clone();
//                        inverseImage.setImageName(name);
//                        inverseImage.resetVOIs();
//
//                        // Make algorithm
//                        FFTAlgo = new AlgorithmFFT(inverseImage, resultImage,
//                                transformDir, logMagDisplay, unequalDim,
//                                image25D, imageCrop, kernelDiameter,
//                                filterType, freq1, freq2, constructionMethod,
//                                butterworthOrder);
//                        FFTAlgo.calcStoreInDest();
//
//                    } catch (OutOfMemoryError e) {
//                        displayError("AlgorithmFFT: unable to allocate enough memory");
//
//                        if (inverseImage != null) {
//                            inverseImage.disposeLocal(); // Clean up memory
//                                                            // of result image
//                            inverseImage = null;
//                        }
//
//                        return;
//                    }
//
//                    try {
//                        inverseImage.exportData(0, arrayLength, a);
//                    } catch (IOException e) {
//                        displayError("IOException error on inverseImage.exportData(0, arrayLength, a)");
//                        return;
//                    }
//                } else {
//
//                    try {
//
//                        // No need to make new image space because the user has
//                        // choosen to replace the source image
//                        // Make the algorithm class
//                        FFTAlgo = new AlgorithmFFT(forwardImage, transformDir,
//                                logMagDisplay, unequalDim, image25D, imageCrop,
//                                kernelDiameter, filterType, freq1, freq2,
//                                constructionMethod, butterworthOrder);
//
//                        FFTAlgo.calcInPlace();
//                    } catch (OutOfMemoryError e) {
//                        displayError("AlgorithmFFT: unable to allocate enough memory");
//
//                        return;
//                    }
//
//                    try {
//                        forwardImage.exportData(0, arrayLength, a);
//                    } catch (IOException e) {
//                        displayError("IOException error on forwardImage.exportData(0, arrayLength, a)");
//                        return;
//                    }
//                }
//
//                FFTAlgo.finalize();
//                if (forwardImage != null) {
//                    forwardImage.disposeLocal();
//                    forwardImage = null;
//                }
//                if (resultImage != null) {
//                    resultImage.disposeLocal();
//                    resultImage = null;
//                }
//                if (inverseImage != null) {
//                    inverseImage.disposeLocal();
//                    inverseImage = null;
//                }
//
//                testShift(c, extents, s);
//
//                error = rms(a, c, arrayLength);
//                if (error[0] >= 2.0E-7) {
//                    foundError[i] = true;
//                }
//
//                for (j = 0; j < arrayLength; j++) {
//                    a[j] = Math.abs(a[j] - c[j]);
//                } // for (j = 0; j < n; j++)
//
//                shellSort(a);
//                if (a[arrayLength - 1] >= 1.0E-6) {
//                    foundError[i] = true;
//                }
//
//                if (foundError[i]) {
//                    errorsFound++;
//                    UI.setDataText("Test = " + i + " rms error = " + error[0]
//                            + "\n");
//                    UI.setDataText("Test = " + i
//                            + " the 10 largest error differences\n");
//                    for (j = 0; j < 10; j++) {
//                        UI.setDataText("Diff[" + j + "] = "
//                                + a[arrayLength - 1 - j] + "\n");
//                    }
//                }
//            } // if (testType == SPATIAL_SHIFT)
//            else if (testType == FREQUENCY_SHIFT) {
//                // Test f(x,y) * ((-1)**(x+y)) <=> F(u - extents[0]/2, v -
//                // extents[1]/2)
//                b = new float[arrayLength];
//                d = new float[arrayLength];
//                if (createNewImage) {
//                    try {
//                        resultImage.exportComplexData(0, arrayLength, a, b);
//                    } catch (IOException e) {
//                        displayError("IOException error on resultImage.exportComplexData(0, arrayLength, a, b)");
//                        return;
//                    }
//                } // if (createNewImage)
//                else { // not createNewImage
//                    try {
//                        forwardImage.exportComplexData(0, arrayLength, a, b);
//                    } catch (IOException e) {
//                        displayError("IOException error on forwardImage.exportComplexData(0, arrayLength, a, b)");
//                        return;
//                    }
//                    forwardImage.reallocate(ModelStorageBase.FLOAT);
//                } // else not createNewImage
//                testCenter(a, b, extents);
//                if (nDims == 2) {
//                    for (y = 0; y < extents[1]; y++) {
//                        for (x = 0; x < extents[0]; x++) {
//                            if (((x + y) % 2) == 1) {
//                                c[x + y * extents[0]] *= -1;
//                            }
//                        }
//                    }
//                } else {
//                    for (z = 0; z < extents[2]; z++) {
//                        for (y = 0; y < extents[1]; y++) {
//                            for (x = 0; x < extents[0]; x++) {
//                                if (((x + y + z) % 2) == 1) {
//                                    c[x + y * extents[0] + z * sliceSize] *= -1;
//                                }
//                            }
//                        }
//                    }
//                }
//
//                try {
//                    forwardImage.importData(0, c, true);
//                } catch (IOException e) {
//                    displayError("IOException on forwardImage.importData(0, c, true)");
//                }
//
//                forwardImage.setOriginalCropCheckbox(imageCrop);
//
//                transformDir = FORWARD;
//
//                if (createNewImage) {
//
//                    try {
//
//                        // Make algorithm
//                        FFTAlgo = new AlgorithmFFT(resultImage, forwardImage,
//                                transformDir, logMagDisplay, unequalDim,
//                                image25D, imageCrop, kernelDiameter,
//                                filterType, freq1, freq2, constructionMethod,
//                                butterworthOrder);
//                        FFTAlgo.calcStoreInDest();
//
//                    } catch (OutOfMemoryError e) {
//                        displayError("AlgorithmFFT: unable to allocate enough memory");
//
//                        if (resultImage != null) {
//                            resultImage.disposeLocal(); // Clean up memory of
//                                                        // result image
//                            resultImage = null;
//                        }
//
//                        return;
//                    }
//                } else {
//
//                    try {
//
//                        // No need to make new image space because the user has
//                        // choosen to replace the source image
//                        // Make the algorithm class
//                        FFTAlgo = new AlgorithmFFT(forwardImage, transformDir,
//                                logMagDisplay, unequalDim, image25D, imageCrop,
//                                kernelDiameter, filterType, freq1, freq2,
//                                constructionMethod, butterworthOrder);
//
//                        FFTAlgo.calcInPlace();
//                    } catch (OutOfMemoryError e) {
//                        displayError("AlgorithmFFT: unable to allocate enough memory");
//
//                        return;
//                    }
//                }
//
//                FFTAlgo.finalize();
//
//                if (createNewImage) {
//                    try {
//                        resultImage.exportComplexData(0, arrayLength, c, d);
//                    } catch (IOException e) {
//                        displayError("IOException error on resultImage.exportComplexData(0, arrayLength, c, d)");
//                        return;
//                    }
//                } // if (createNewImage)
//                else { // not createNewImage
//                    try {
//                        forwardImage.exportComplexData(0, arrayLength, c, d);
//                    } catch (IOException e) {
//                        displayError("IOException error on forwardImage.exportComplexData(0, arrayLength, c, d)");
//                        return;
//                    }
//                } // else not createNewImage
//
//                if (forwardImage != null) {
//                    forwardImage.disposeLocal();
//                }
//                if (resultImage != null) {
//                    resultImage.disposeLocal();
//                }
//
//                error = rms(a, b, c, d, arrayLength);
//                if ((error[0] >= 2.0E-7) || (error[1] > 2.0E-7)) {
//                    foundError[i] = true;
//                }
//
//                for (j = 0; j < arrayLength; j++) {
//                    a[j] = (float) Math.sqrt((a[j] - c[j]) * (a[j] - c[j])
//                            + (b[j] - d[j]) * (b[j] - d[j]));
//                } // for (j = 0; j < n; j++)
//
//                shellSort(a);
//                if (a[arrayLength - 1] >= 1.0E-6) {
//                    foundError[i] = true;
//                }
//
//                if (foundError[i]) {
//                    errorsFound++;
//                    UI.setDataText("Test = " + i + " rms errors = " + error[0]
//                            + " , " + error[1] + "\n");
//                    UI.setDataText("Test = " + i
//                            + " the 10 largest error differences\n");
//                    for (j = 0; j < 10; j++) {
//                        UI.setDataText("Diff[" + j + "] = "
//                                + a[arrayLength - 1 - j] + "\n");
//                    }
//                }
//            } // else if (testType == FREQUENCY_SHIFT)
//            else { // testType == FORWARD_INVERSE
//                transformDir = INVERSE;
//
//                if (createNewImage) {
//
//                    try {
//                        inverseImage = (ModelImage) resultImage.clone();
//                        inverseImage.setImageName(name);
//                        inverseImage.resetVOIs();
//
//                        // Make algorithm
//                        FFTAlgo = new AlgorithmFFT(inverseImage, resultImage,
//                                transformDir, logMagDisplay, unequalDim,
//                                image25D, imageCrop, kernelDiameter,
//                                filterType, freq1, freq2, constructionMethod,
//                                butterworthOrder);
//                        FFTAlgo.calcStoreInDest();
//
//                    } catch (OutOfMemoryError e) {
//                        displayError("AlgorithmFFT: unable to allocate enough memory");
//
//                        if (inverseImage != null) {
//                            inverseImage.disposeLocal(); // Clean up memory
//                                                            // of result image
//                            inverseImage = null;
//                        }
//
//                        return;
//                    }
//
//                    try {
//                        inverseImage.exportData(0, arrayLength, a);
//                    } catch (IOException e) {
//                        displayError("IOException error on inverseImage.exportData(0, arrayLength, a)");
//                        return;
//                    }
//                } else {
//
//                    try {
//
//                        // No need to make new image space because the user has
//                        // choosen to replace the source image
//                        // Make the algorithm class
//                        FFTAlgo = new AlgorithmFFT(forwardImage, transformDir,
//                                logMagDisplay, unequalDim, image25D, imageCrop,
//                                kernelDiameter, filterType, freq1, freq2,
//                                constructionMethod, butterworthOrder);
//
//                        FFTAlgo.calcInPlace();
//                    } catch (OutOfMemoryError e) {
//                        displayError("Dialog FFT: unable to allocate enough memory");
//
//                        return;
//                    }
//
//                    try {
//                        forwardImage.exportData(0, arrayLength, a);
//                    } catch (IOException e) {
//                        displayError("IOException error on forwardImage.exportData(0, arrayLength, a)");
//                        return;
//                    }
//                }
//
//                FFTAlgo.finalize();
//                if (forwardImage != null) {
//                    forwardImage.disposeLocal();
//                    forwardImage = null;
//                }
//                if (resultImage != null) {
//                    resultImage.disposeLocal();
//                    resultImage = null;
//                }
//                if (inverseImage != null) {
//                    inverseImage.disposeLocal();
//                    inverseImage = null;
//                }
//
//                if (doCrop) {
//                    j = 0;
//                    if (nDims == 2) {
//                        newArrayLength = (end[0] - start[0] + 1)
//                                * (end[1] - start[1] + 1);
//                        newA = new float[newArrayLength];
//                        newC = new float[newArrayLength];
//                        for (y = start[1]; y <= end[1]; y++) {
//                            for (x = start[0]; x <= end[0]; x++) {
//                                newA[j] = a[x + y * extents[0]];
//                                newC[j++] = c[x + y * extents[0]];
//                            }
//                        }
//                    } // if (nDims == 2)
//                    else {
//                        newArrayLength = (end[0] - start[0] + 1)
//                                * (end[1] - start[1] + 1)
//                                * (end[2] - start[2] + 1);
//                        newA = new float[newArrayLength];
//                        newC = new float[newArrayLength];
//                        for (z = start[2]; z <= end[2]; z++) {
//                            for (y = start[1]; y <= end[1]; y++) {
//                                for (x = start[0]; x <= end[0]; x++) {
//                                    newA[j] = a[x + y * extents[0] + z
//                                            * sliceSize];
//                                    newC[j++] = c[x + y * extents[0] + z
//                                            * sliceSize];
//                                }
//                            }
//                        }
//                    }
//                    a = new float[newArrayLength];
//                    c = new float[newArrayLength];
//                    for (j = 0; j < newArrayLength; j++) {
//                        a[j] = newA[j];
//                        c[j] = newC[j];
//                    }
//                    arrayLength = newArrayLength;
//                } // if (doCrop)
//
//                error = rms(a, c, arrayLength);
//                if (error[0] >= 2.0E-7) {
//                    foundError[i] = true;
//                }
//
//                for (j = 0; j < arrayLength; j++) {
//                    a[j] = Math.abs(a[j] - c[j]);
//                } // for (j = 0; j < n; j++)
//
//                shellSort(a);
//                if (a[arrayLength - 1] >= 1.0E-6) {
//                    foundError[i] = true;
//                }
//
//                if (foundError[i]) {
//                    errorsFound++;
//                    UI.setDataText("Test = " + i + " rms error = " + error[0]
//                            + "\n");
//                    UI.setDataText("Test = " + i
//                            + " the 10 largest error differences\n");
//                    for (j = 0; j < 10; j++) {
//                        UI.setDataText("Diff[" + j + "] = "
//                                + a[arrayLength - 1 - j] + "\n");
//                    }
//                }
//            } // else testType == FORWARD_INVERSE
//
//        } // for (i = 0; i < 136; i++)
//
//        testType = FREQUENCY_SHIFT;
//        // f(x,y) * exp(-j*2*PI*(u0*x/M + v0*y/N)) <=> F(u - u0, v - v0)
//        // Let u0 = s*M, v0 = s*N
//        // f(x,y) * exp(-j*2*PI*s*(x + y)) <=> F(u - s*M, v - s*N)
//        // Re(f(x,y))*cos(2*PI*s*(x+y) + Im(f(x,y))*sin(2*PI*s*(x+y))
//        // + j*[-Re(f(x,y))*sin(2*PI*s*(x+y)) + Im(f(x,y))*cos(2*PI*s*(x+y))]
//        for (i = 136; i < nTests; i++) {
//            UI.setDataText("Running test = " + i + "\n");
//            if (i == 136) {
//                s = 0.25;
//                createNewImage = true;
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 256;
//                extents[1] = 256;
//                constructionMethod = GAUSSIAN;
//            } else if (i == 137) {
//                s = 0.25;
//                createNewImage = false;
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 256;
//                extents[1] = 256;
//                constructionMethod = GAUSSIAN;
//            } else if (i == 138) {
//                s = 0.25;
//                createNewImage = true;
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                constructionMethod = GAUSSIAN;
//                image25D = false;
//            } else if (i == 139) {
//                s = 0.25;
//                createNewImage = false;
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                constructionMethod = GAUSSIAN;
//                image25D = false;
//            } else if (i == 140) {
//                s = 0.5;
//                createNewImage = true;
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 256;
//                extents[1] = 256;
//                constructionMethod = GAUSSIAN;
//            } else if (i == 141) {
//                s = 0.5;
//                createNewImage = false;
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 256;
//                extents[1] = 256;
//                constructionMethod = GAUSSIAN;
//            } else if (i == 142) {
//                s = 0.5;
//                createNewImage = true;
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                constructionMethod = GAUSSIAN;
//                image25D = false;
//            } else if (i == 143) {
//                s = 0.5;
//                createNewImage = false;
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                constructionMethod = GAUSSIAN;
//                image25D = false;
//            } else if (i == 144) {
//                s = 0.75;
//                createNewImage = true;
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 256;
//                extents[1] = 256;
//                constructionMethod = GAUSSIAN;
//            } else if (i == 145) {
//                s = 0.75;
//                createNewImage = false;
//                nDims = 2;
//                extents = new int[nDims];
//                extents[0] = 256;
//                extents[1] = 256;
//                constructionMethod = GAUSSIAN;
//            } else if (i == 146) {
//                s = 0.75;
//                createNewImage = true;
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                constructionMethod = GAUSSIAN;
//                image25D = false;
//            } else if (i == 147) {
//                s = 0.75;
//                createNewImage = false;
//                nDims = 3;
//                extents = new int[nDims];
//                extents[0] = 128;
//                extents[1] = 128;
//                extents[2] = 128;
//                constructionMethod = GAUSSIAN;
//                image25D = false;
//            }
//
//            if (nDims == 3) {
//                sliceSize = extents[0] * extents[1];
//            }
//
//            arrayLength = extents[0];
//            for (j = 1; j < nDims; j++) {
//                arrayLength *= extents[j];
//            }
//
//            a = new float[arrayLength];
//            b = new float[arrayLength];
//            c = new float[arrayLength];
//            d = new float[arrayLength];
//
//            for (j = 0; j < arrayLength; j++) {
//                c[j] = randomGen.genUniformRandomNum(0.0f, 1.0f);
//                a[j] = c[j];
//                d[j] = randomGen.genUniformRandomNum(0.0f, 1.0f);
//                b[j] = d[j];
//            } // for (j = 0; j < arrayLength; j++)
//
//            forwardImage = new ModelImage(ModelStorageBase.COMPLEX, extents,
//                    "forwardImage");
//
//            try {
//                forwardImage.importComplexData(0, a, b, true, true);
//            } catch (IOException e) {
//                displayError("IOException on forwardImage.importComplexData(0, a, b, true, true)");
//            }
//
//            String name = forwardImage.getImageName() + "_FFT";
//            transformDir = FORWARD;
//
//            if (createNewImage) {
//
//                try {
//                    resultImage = (ModelImage) forwardImage.clone();
//                    resultImage.setImageName(name);
//                    resultImage.resetVOIs();
//
//                    // Make algorithm
//                    FFTAlgo = new AlgorithmFFT(resultImage, forwardImage,
//                            transformDir, logMagDisplay, unequalDim, image25D,
//                            imageCrop, kernelDiameter, filterType, freq1,
//                            freq2, constructionMethod, butterworthOrder);
//                    FFTAlgo.calcStoreInDest();
//
//                } catch (OutOfMemoryError e) {
//                    displayError("AlgorithmFFT: unable to allocate enough memory");
//
//                    if (resultImage != null) {
//                        resultImage.disposeLocal(); // Clean up memory of result
//                                                    // image
//                        resultImage = null;
//                    }
//
//                    return;
//                }
//            } else {
//
//                try {
//
//                    // No need to make new image space because the user has
//                    // choosen to replace the source image
//                    // Make the algorithm class
//                    FFTAlgo = new AlgorithmFFT(forwardImage, transformDir,
//                            logMagDisplay, unequalDim, image25D, imageCrop,
//                            kernelDiameter, filterType, freq1, freq2,
//                            constructionMethod, butterworthOrder);
//
//                    FFTAlgo.calcInPlace();
//                } catch (OutOfMemoryError e) {
//                    displayError("AlgorithmFFT: unable to allocate enough memory");
//
//                    return;
//                }
//            }
//
//            FFTAlgo.finalize();
//
//            if (createNewImage) {
//                try {
//                    resultImage.exportComplexData(0, arrayLength, a, b);
//                } catch (IOException e) {
//                    displayError("IOException error on resultImage.exportComplexData(0, arrayLength, a, b)");
//                    return;
//                }
//            } // if (createNewImage)
//            else { // not createNewImage
//                try {
//                    forwardImage.exportComplexData(0, arrayLength, a, b);
//                } catch (IOException e) {
//                    displayError("IOException error on forwardImage.exportComplexData(0, arrayLength, a, b)");
//                    return;
//                }
//            } // else not createNewImage
//
//            testCenter(a, b, extents);
//            testShift(a, b, extents, s);
//
//            if (nDims == 2) {
//                for (y = 0; y < extents[1]; y++) {
//                    for (x = 0; x < extents[0]; x++) {
//                        arg = 2.0 * Math.PI * s * (x + y);
//                        cosxy = Math.cos(arg);
//                        sinxy = Math.sin(arg);
//                        index = x + y * extents[0];
//                        realF = c[index] * cosxy + d[index] * sinxy;
//                        d[index] = (float) (-c[index] * sinxy + d[index]
//                                * cosxy);
//                        c[index] = (float) realF;
//                    }
//                }
//            } else {
//                for (z = 0; z < extents[2]; z++) {
//                    for (y = 0; y < extents[1]; y++) {
//                        for (x = 0; x < extents[0]; x++) {
//                            arg = 2.0 * Math.PI * s * (x + y + z);
//                            cosxy = Math.cos(arg);
//                            sinxy = Math.sin(arg);
//                            index = x + y * extents[0] + z * sliceSize;
//                            realF = c[index] * cosxy + d[index] * sinxy;
//                            d[index] = (float) (-c[index] * sinxy + d[index]
//                                    * cosxy);
//                            c[index] = (float) realF;
//                        }
//                    }
//                }
//            }
//
//            try {
//                forwardImage.importComplexData(0, c, d, true, true);
//            } catch (IOException e) {
//                displayError("IOException on forwardImage.importComplexData(0, c, d, true, true)");
//            }
//
//            transformDir = FORWARD;
//
//            if (createNewImage) {
//
//                try {
//
//                    // Make algorithm
//                    FFTAlgo = new AlgorithmFFT(resultImage, forwardImage,
//                            transformDir, logMagDisplay, unequalDim, image25D,
//                            imageCrop, kernelDiameter, filterType, freq1,
//                            freq2, constructionMethod, butterworthOrder);
//                    FFTAlgo.calcStoreInDest();
//
//                } catch (OutOfMemoryError e) {
//                    displayError("AlgorithmFFT: unable to allocate enough memory");
//
//                    if (resultImage != null) {
//                        resultImage.disposeLocal(); // Clean up memory of result
//                                                    // image
//                        resultImage = null;
//                    }
//
//                    return;
//                }
//            } else {
//
//                try {
//
//                    // No need to make new image space because the user has
//                    // choosen to replace the source image
//                    // Make the algorithm class
//                    FFTAlgo = new AlgorithmFFT(forwardImage, transformDir,
//                            logMagDisplay, unequalDim, image25D, imageCrop,
//                            kernelDiameter, filterType, freq1, freq2,
//                            constructionMethod, butterworthOrder);
//
//                    FFTAlgo.calcInPlace();
//                } catch (OutOfMemoryError e) {
//                    displayError("AlgorithmFFT: unable to allocate enough memory");
//
//                    return;
//                }
//            }
//
//            FFTAlgo.finalize();
//
//            if (createNewImage) {
//                try {
//                    resultImage.exportComplexData(0, arrayLength, c, d);
//                } catch (IOException e) {
//                    displayError("IOException error on resultImage.exportComplexData(0, arrayLength, c, d)");
//                    return;
//                }
//            } // if (createNewImage)
//            else { // not createNewImage
//                try {
//                    forwardImage.exportComplexData(0, arrayLength, c, d);
//                } catch (IOException e) {
//                    displayError("IOException error on forwardImage.exportComplexData(0, arrayLength, c, d)");
//                    return;
//                }
//            } // else not createNewImage
//
//            if (forwardImage != null) {
//                forwardImage.disposeLocal();
//            }
//            if (resultImage != null) {
//                resultImage.disposeLocal();
//            }
//
//            testCenter(c, d, extents);
//            error = rms(a, b, c, d, arrayLength);
//            if ((error[0] >= 2.0E-7) || (error[1] > 2.0E-7)) {
//                foundError[i] = true;
//            }
//
//            for (j = 0; j < arrayLength; j++) {
//                a[j] = (float) Math.sqrt((a[j] - c[j]) * (a[j] - c[j])
//                        + (b[j] - d[j]) * (b[j] - d[j]));
//            } // for (j = 0; j < n; j++)
//
//            shellSort(a);
//            if (a[arrayLength - 1] >= 1.0E-6) {
//                foundError[i] = true;
//            }
//
//            if (foundError[i]) {
//                errorsFound++;
//                UI.setDataText("Test = " + i + " rms errors = " + error[0]
//                        + " , " + error[1] + "\n");
//                UI.setDataText("Test = " + i
//                        + " the 10 largest error differences\n");
//                for (j = 0; j < 10; j++) {
//                    UI.setDataText("Diff[" + j + "] = "
//                            + a[arrayLength - 1 - j] + "\n");
//                }
//            }
//        } // for (i = 136; i < ntests; i++)
//        UI.setDataText("Errors were found in " + errorsFound + " of " + nTests
//                + " tests\n");
//        setCompleted(false);
//        return;
//    }
//
//    private void testShift(float[] a, int[] extents, double s) {
//        int x, y, z;
//        int oldX, oldY, oldZ;
//        float newA[] = new float[a.length];
//        int sliceSize;
//        int nDims = extents.length;
//
//        if (nDims == 2) {
//            for (y = 0; y < extents[1]; y++) {
//                if ((Math.round(y - s * extents[1])) >= 0) {
//                    oldY = (int) Math.round(y - s * extents[1]);
//                } else {
//                    oldY = (int) Math.round(y + (1 - s) * extents[1]);
//                }
//                for (x = 0; x < extents[0]; x++) {
//                    if ((Math.round(x - s * extents[0])) >= 0) {
//                        oldX = (int) Math.round(x - s * extents[0]);
//                    } else {
//                        oldX = (int) Math.round(x + (1 - s) * extents[0]);
//                    }
//                    newA[x + extents[0] * y] = a[oldX + extents[0] * oldY];
//                }
//            }
//        } // if (nDims == 2)
//        else { // nDims == 3
//            sliceSize = extents[0] * extents[1];
//            for (z = 0; z < extents[2]; z++) {
//                if ((Math.round(z - s * extents[2])) >= 0) {
//                    oldZ = (int) Math.round(z - s * extents[2]);
//                } else {
//                    oldZ = (int) Math.round(z + (1 - s) * extents[2]);
//                }
//                for (y = 0; y < extents[1]; y++) {
//                    if ((Math.round(y - s * extents[1])) >= 0) {
//                        oldY = (int) Math.round(y - s * extents[1]);
//                    } else {
//                        oldY = (int) Math.round(y + (1 - s) * extents[1]);
//                    }
//                    for (x = 0; x < extents[0]; x++) {
//                        if ((Math.round(x - s * extents[0])) >= 0) {
//                            oldX = (int) Math.round(x - s * extents[0]);
//                        } else {
//                            oldX = (int) Math.round(x + (1 - s) * extents[0]);
//                        }
//                        newA[x + extents[0] * y + sliceSize * z] = a[oldX
//                                + extents[0] * oldY + sliceSize * oldZ];
//                    }
//                }
//            }
//        } // else nDims == 3
//        for (x = 0; x < a.length; x++) {
//            a[x] = newA[x];
//        }
//    }
//
//    private void testShift(float[] a, float[] b, int[] extents, double s) {
//        int x, y, z;
//        int oldX, oldY, oldZ;
//        float newA[] = new float[a.length];
//        float newB[] = new float[b.length];
//        int sliceSize;
//        int nDims = extents.length;
//
//        if (nDims == 2) {
//            for (y = 0; y < extents[1]; y++) {
//                if ((Math.round(y - s * extents[1])) >= 0) {
//                    oldY = (int) Math.round(y - s * extents[1]);
//                } else {
//                    oldY = (int) Math.round(y + (1 - s) * extents[1]);
//                }
//                for (x = 0; x < extents[0]; x++) {
//                    if ((Math.round(x - s * extents[0])) >= 0) {
//                        oldX = (int) Math.round(x - s * extents[0]);
//                    } else {
//                        oldX = (int) Math.round(x + (1 - s) * extents[0]);
//                    }
//                    newA[x + extents[0] * y] = a[oldX + extents[0] * oldY];
//                    newB[x + extents[0] * y] = b[oldX + extents[0] * oldY];
//                }
//            }
//        } // if (nDims == 2)
//        else { // nDims == 3
//            sliceSize = extents[0] * extents[1];
//            for (z = 0; z < extents[2]; z++) {
//                if ((Math.round(z - s * extents[2])) >= 0) {
//                    oldZ = (int) Math.round(z - s * extents[2]);
//                } else {
//                    oldZ = (int) Math.round(z + (1 - s) * extents[2]);
//                }
//                for (y = 0; y < extents[1]; y++) {
//                    if ((Math.round(y - s * extents[1])) >= 0) {
//                        oldY = (int) Math.round(y - s * extents[1]);
//                    } else {
//                        oldY = (int) Math.round(y + (1 - s) * extents[1]);
//                    }
//                    for (x = 0; x < extents[0]; x++) {
//                        if ((Math.round(x - s * extents[0])) >= 0) {
//                            oldX = (int) Math.round(x - s * extents[0]);
//                        } else {
//                            oldX = (int) Math.round(x + (1 - s) * extents[0]);
//                        }
//                        newA[x + extents[0] * y + sliceSize * z] = a[oldX
//                                + extents[0] * oldY + sliceSize * oldZ];
//                        newB[x + extents[0] * y + sliceSize * z] = b[oldX
//                                + extents[0] * oldY + sliceSize * oldZ];
//                    }
//                }
//            }
//        } // else nDims == 3
//        for (x = 0; x < a.length; x++) {
//            a[x] = newA[x];
//            b[x] = newB[x];
//        }
//    }
//
//    private void testCenter(float[] a, float[] b, int[] extents) {
//        int x, y, z;
//        int oldX, oldY, oldZ;
//        float newA[] = new float[a.length];
//        float newB[] = new float[b.length];
//        int sliceSize;
//        int nDims = extents.length;
//
//        if (nDims == 2) {
//            for (y = 0; y < extents[1]; y++) {
//                if ((y - extents[1] / 2) >= 0) {
//                    oldY = y - extents[1] / 2;
//                } else {
//                    oldY = y + extents[1] / 2;
//                }
//                for (x = 0; x < extents[0]; x++) {
//                    if ((x - extents[0] / 2) >= 0) {
//                        oldX = x - extents[0] / 2;
//                    } else {
//                        oldX = x + extents[0] / 2;
//                    }
//                    newA[x + extents[0] * y] = a[oldX + extents[0] * oldY];
//                    newB[x + extents[0] * y] = b[oldX + extents[0] * oldY];
//                }
//            }
//        } // if (nDims == 2)
//        else { // nDims == 3
//            sliceSize = extents[0] * extents[1];
//            for (z = 0; z < extents[2]; z++) {
//                if ((z - extents[2] / 2) >= 0) {
//                    oldZ = z - extents[2] / 2;
//                } else {
//                    oldZ = z + extents[2] / 2;
//                }
//                for (y = 0; y < extents[1]; y++) {
//                    if ((y - extents[1] / 2) >= 0) {
//                        oldY = y - extents[1] / 2;
//                    } else {
//                        oldY = y + extents[1] / 2;
//                    }
//                    for (x = 0; x < extents[0]; x++) {
//                        if ((x - extents[0] / 2) >= 0) {
//                            oldX = x - extents[0] / 2;
//                        } else {
//                            oldX = x + extents[0] / 2;
//                        }
//                        newA[x + extents[0] * y + sliceSize * z] = a[oldX
//                                + extents[0] * oldY + sliceSize * oldZ];
//                        newB[x + extents[0] * y + sliceSize * z] = b[oldX
//                                + extents[0] * oldY + sliceSize * oldZ];
//                    }
//                }
//            }
//        } // else nDims == 3
//        for (x = 0; x < a.length; x++) {
//            a[x] = newA[x];
//            b[x] = newB[x];
//        }
//    }
//
}
