package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;


import gov.nih.mipav.model.structures.TransMatrix;

import Jama.Matrix;

import gov.nih.mipav.model.algorithms.*;


/**
*
* The DTI gradient table correction after transformation algorithm corrects gradients after their corresponding
* DWI volumes have been registered using OAR35D to a B0 image.
* 
* 
* <hr>
* The copyright below only pertains to this algorithm in its entirety. Portions of the original code have been
* modified in order to relate to MIPAV structures. 
* 
* <pre>
* Copyright (c) 2011, Bennett Landman
* All rights reserved.
* Redistribution and use in source and binary forms, with or without 
* modification, are permitted provided that the following conditions are met:
* 
*      - Redistributions of source code must retain the above copyright 
*        notice, this list of conditions and the following disclaimer.
*        
*      - Redistributions in binary form must reproduce the above copyright 
*        notice, this list of conditions and the following disclaimer in the 
*        documentation and/or other materials provided with the distribution.
*        
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY 
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
* OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT 
* SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT 
* OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
* HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR 
* TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, 
* EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
* 
* </pre>
* 
* 
* @version 0.1 March 2, 2012
* @author Beth Tyrie
* */

public class DTIGradTableCorrectionAfterTrans extends AlgorithmBase {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    private float[][] gradients;

    private float[] bValues;

    private TransMatrix[] arrayTransMatrix;
    
    private int refImageNum;

    float[][] angCorrGT;

    Matrix[] gradMatArray;


    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    public DTIGradTableCorrectionAfterTrans(float[][] gradients, float[] bValues, TransMatrix[] arrayTransMatrix, int refImageNum) {
        super();
        this.gradients = gradients;
        this.bValues = bValues;
        this.arrayTransMatrix = arrayTransMatrix;
        this.refImageNum = refImageNum;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
    }
    
    public float[][] getCorrectedGradients() {
        return angCorrGT;
    }

    /**
     * Runs algorithm.
     */
    public void runAlgorithm() {

        gradMatArray = new Matrix[bValues.length];

        for (int i = 0; i < bValues.length; i++) {
            gradMatArray[i] = new Matrix(1, 3);
            gradMatArray[i].set(0, 0, gradients[i][0]);
            gradMatArray[i].set(0, 1, gradients[i][1]);
            gradMatArray[i].set(0, 2, gradients[i][2]);
        }

        angCorrGT = new float[bValues.length][3];
        System.out.println("angCoorGT.length" +angCorrGT.length);

        int bOcount = 0;
        for (int i = 0; i < arrayTransMatrix.length; i++) {
           

            float[][] row = { {gradients[i][0]}, {gradients[i][1]}, {gradients[i][2]}};
            if (i == refImageNum && (row[0][0] == 0 && row[1][0] == 0 && row[2][0] == 0)
                    || (row[0][0] == 100 && row[1][0] == 100 && row[2][0] == 100 || 
                            (row[0][0] == 1000 && row[1][0] == 1000 && row[2][0] == 1000))) {
                angCorrGT[i][0] = row[0][0];
                angCorrGT[i][1] = row[1][0];
                angCorrGT[i][2] = row[2][0];
                if (i == refImageNum ){
                    bOcount = 1 ;
                }
            } 
            
            else {
                TransMatrix A;
                
                 A = arrayTransMatrix[i-bOcount];
                 System.out.println("i: " +i);
                 System.out.println("i-b0Count" +(i-bOcount));
                 System.out.println("arrayTransMatrix: " +arrayTransMatrix[i-bOcount]);
                 float[][] newrow = matrixMultiply(A,row);
                 angCorrGT[i][0]=newrow[0][0];
                 angCorrGT[i][1]=newrow[1][0];
                 angCorrGT[i][2]=newrow[2][0];
            }

        }

    }
    

    private float[][] matrixMultiply(TransMatrix A, float[][] B) {
        float[][] C = new float[4][B[0].length];
        for (int i = 0; i < C.length; i++) {
            for (int j = 0; j < C[0].length; j++) {
                C[i][j] = (A.Get(i, 0)* B[0][j]) + (A.Get(i, 1) * B[1][j]) + (A.Get(i, 2) * B[2][j]);
            }
        }
        return C;
    }

}
