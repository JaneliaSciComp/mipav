



import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmRegionGrow;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.filters.AlgorithmGradientMagnitudeSep;

import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR2D;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmConcat;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import java.awt.Point;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.BitSet;
import javax.swing.JTextArea;
import java.lang.Math;

public class PlugInAlgorithmNEIRetinalRegistration extends AlgorithmBase {
    
    /** Path of first directory */
    private String imagePath1; 
    
    /** Path of second directory */
    private String imagePath2;
    
    /** Path of reference directory */
    private String refPath;
    
    /** Output box */
    private JTextArea outputbox;
    
    /** Reference Image */
    private ModelImage reference;
    
    /** Array holding nonRegistered Images **/
    ArrayList notReg = new ArrayList();
    
    /** Cost of current images **/
    private double currCost; //alert
    
    /** Dimensions of blur for Gradient Mag **/
    private float[] sigma = new float[2];
 
    /** bitset of eye **/
    private BitSet eyebitset = new BitSet();  

    /** Algorithm which converts to float **/
    AlgorithmChangeType algoType;
    
    
    FileIO fileIO = new FileIO();
    
    /** Stores concatnated image **/
    ModelImage concated;
    
    /** if true generates concatnated image **/
    private boolean shouldConcat;    
    
    /** concatnated algorithm **/
    AlgorithmConcat algoConcat;
    
    /** user inputed variables **/
    private float epsY, epsB;
    
    /** Mean MPmap image **/
    private ModelImage avg;
    
    private float dPerP;
    

    public PlugInAlgorithmNEIRetinalRegistration(String imageDir1, String imageDir2, JTextArea outputbox, String refPath, boolean toConcat, float epsY, float epsB, float dPerP) {
        sigma[0] = 6;
        sigma[1] = 6;
        this.imagePath1 = imageDir1;
        this.imagePath2 = imageDir2;
        this.refPath = refPath;
        this.outputbox = outputbox;
        this.shouldConcat = toConcat;
        this.epsY = epsY;
        this.epsB = epsB;
        this.dPerP = dPerP;
 
    }

    @Override
    public void runAlgorithm() {
        
        ArrayList imgLoc1= new ArrayList(), imgLoc2 = new ArrayList();
        File  f1 = new File(imagePath1);
        File  f2 = new File(imagePath2);   
        
        //Make list of files in Directory 1 to check in Dir 2
        File[] listOfFiles1 = f1.listFiles() ;
        File[] listOfFiles2 = f2.listFiles() ;        
        
        //For each image in directory 1 load into array

        for (int i = 0; i< listOfFiles1.length; i++){
            File testFile  = new File (f1.getPath(), listOfFiles1[i].getName());
            
            if (testFile.exists() && testFile.getName().endsWith(".tif")){
                //If there is an image save both to list arrays
                imgLoc1.add(testFile.getAbsolutePath());
                
            }
        }
        for (int i = 0; i< listOfFiles2.length; i++){
            File testFile  = new File (f2.getPath(), listOfFiles2[i].getName());
            
            if (testFile.exists() && testFile.getName().endsWith(".tif")){
                //If there is an image save both to list arrays
                imgLoc2.add(testFile.getAbsolutePath());
                
            }
        }
            
            //Read in reference
            outputbox.append("** Loading " + refPath + " in as Reference Image: \n");
            fileIO.setQuiet(true);
            reference = fileIO.readImage(refPath);
            reference.setImageName("reference");
        
        
        //make 3 new folders
        File outputfolder = new File(f2.getParent(), "Output");
        outputfolder.mkdir();
        
        File outputfolder2 = new File(f2.getParent(), "YellowReg");
        outputfolder2.mkdir();
        
        File outputfolder3 = new File(f2.getParent(), "BlueReg");
        outputfolder3.mkdir();
        
        //select eye for registration
        eyebitset = eyeballSelect(reference, eyebitset, 7);
        
        
        //file write options
        
        FileWriteOptions opts = new FileWriteOptions(true);
        opts.setFileType(FileUtility.TIFF);
        opts.setBeginSlice(0);
        opts.setEndSlice(0);
        opts.setOptionsSet(true);
        opts.setFileDirectory(outputfolder2 + outputfolder2.separator);
        ModelImage toBeReg, doneReg, convDoneReg;
        outputbox.append("        ** Calculating Gradient Magnitude for Registration**\n");
        //find gradient magnitude
        AlgorithmGradientMagnitudeSep algogMag = new AlgorithmGradientMagnitudeSep(reference, sigma, true, false);
        algogMag.run();
        try{
            reference.importData(0, algogMag.getResultBuffer(), true);
        }catch(IOException e){
            MipavUtil.displayError("FileIO Error");
        }


        
        //for all yellow images
        for(int j = 0; j<imgLoc1.size(); j++){
            outputbox.append("** Loading Yellow " + listOfFiles1[j].getName() + " :\n");
            outputbox.setCaretPosition( outputbox.getText().length() );
            toBeReg = fileIO.readImage((String)imgLoc1.get(j));
            opts.setFileName("YellowRegistered" + (j+1));
            
            //register image
            doneReg = registration(reference, toBeReg);
            
            if (toBeReg != null){
                toBeReg.disposeLocal();
                toBeReg = null;
            }
            
            //build conatnated image
            if (shouldConcat){
                if (j == 0){ //is this the first iamge
                    concated = (ModelImage)doneReg.clone();
                }
                else{
                    int[] dims = new int[3];
                    dims[0] = concated.getExtents()[0];
                    dims[1] = concated.getExtents()[1];
                    if (concated.getExtents().length == 2) //is this the second image
                        dims[2] = 2; //add third dimension
                    else
                        dims[2] = concated.getExtents()[2]+1;
                    
                    ModelImage temp = new ModelImage(ModelStorageBase.USHORT, dims, "Concated");
                    
                    algoConcat = new AlgorithmConcat(doneReg, concated, temp);
                    algoConcat.runAlgorithm();
                    
                    if (concated != null){
                        concated.disposeLocal();
                        concated = null;             
                    }
                    
                    concated = (ModelImage) temp.clone();
                    
                    if (temp != null){
                        temp.disposeLocal();
                        temp = null; 
                    }
                    
                    if (algoConcat != null){
                        algoConcat.finalize();
                        algoConcat = null;                     
                    }

                    
                    
                    
                }
            }
            
            //convert image to float
            convDoneReg = new ModelImage(ModelStorageBase.FLOAT,doneReg.getExtents(), doneReg.getImageName());
            convDoneReg.getFileInfo(0).setEndianess(false);
            
            algoType = new AlgorithmChangeType(convDoneReg, doneReg, doneReg.getMin(), doneReg.getMax(), doneReg.getMin(), doneReg.getMax(), false);
            algoType.run();
            
            //save image
            fileIO.writeImage(convDoneReg, opts);
            if (convDoneReg != null){
                convDoneReg.disposeLocal();
                convDoneReg = null;
            }
            
            if (currCost > .35){  //may not be registered, alert user
                notReg.add("YellowRegistered" + (j+1));
            }
            
            outputbox.append("        ** Cost = " + currCost + " **\n");
            imgLoc1.set(j, new File(outputfolder2, "YellowRegistered" + (j+1)).toString());   
            if (doneReg!=null){
                doneReg.disposeLocal();
                doneReg=null;
            }
            }
        opts.setFileDirectory(outputfolder3 + outputfolder3.separator);
        
        //reister blue images
        for(int j = 0; j<imgLoc2.size(); j++){
            outputbox.append("** Loading Blue " + listOfFiles1[j].getName() + " :\n");
            outputbox.setCaretPosition( outputbox.getText().length() );
            toBeReg = fileIO.readImage((String)imgLoc2.get(j));
            opts.setFileName("BlueRegistered" + (j+1));
            
            //register images
            doneReg = registration(reference, toBeReg);
            
            if (toBeReg != null){
                toBeReg.disposeLocal();
                toBeReg = null;
            }
            
            if (shouldConcat){ //build concatnated image if true
                int[] dims = new int[3];
                dims[0] = concated.getExtents()[0];
                dims[1] = concated.getExtents()[1];
                if (concated.getExtents().length == 2)
                    dims[2] = 2;
                else
                    dims[2] = concated.getExtents()[2]+1;
                
                ModelImage temp = new ModelImage(ModelStorageBase.USHORT, dims, "Concated");
                
                algoConcat = new AlgorithmConcat(doneReg, concated, temp);
                algoConcat.runAlgorithm();
                if (concated != null){
                    concated.disposeLocal();
                    concated = null;             
                }

                concated = (ModelImage) temp.clone();
                if (temp != null){
                    temp.disposeLocal();
                    temp = null; 
                }
                
                if (algoConcat != null){
                    algoConcat.finalize();
                    algoConcat = null;                     
                }
                
            }

        //convert to float
        convDoneReg = new ModelImage(ModelStorageBase.FLOAT,doneReg.getExtents(), doneReg.getImageName());
        convDoneReg.getFileInfo(0).setEndianess(false);
        
        algoType = new AlgorithmChangeType(convDoneReg, doneReg, doneReg.getMin(), doneReg.getMax(), doneReg.getMin(), doneReg.getMax(), false);
        algoType.run();
        
        
        fileIO.writeImage(convDoneReg, opts);
        
        if (convDoneReg != null){
            convDoneReg.disposeLocal();
            convDoneReg = null;
        }
        

        
        if (currCost > .35) //warn user if it may not be registered
            notReg.add("BlueRegistered" + (j+1));

        outputbox.append("        ** Cost= " + currCost + " **\n");
        outputbox.setCaretPosition( outputbox.getText().length() );
        if (doneReg!=null){
            doneReg.disposeLocal();
            doneReg=null;
        }
        imgLoc2.set(j, new File(outputfolder3, "BlueRegistered" + (j+1)).toString());
    }
        if (notReg.size() == 0){//alert
            outputbox.append("** All images are Registered **\n");//alert
            outputbox.setCaretPosition( outputbox.getText().length() );
        }
        else{
            for (int i = 0; i<notReg.size() ; i ++){
                outputbox.append("** " + notReg.get(i) + " does not seemed to be registered, please register manually using reference **\n");
                outputbox.setCaretPosition( outputbox.getText().length() );
            }
        }
        if (shouldConcat){ //show concatnated image
            new ViewJFrameImage(concated);

            }
        if (reference != null){
            reference.disposeLocal();
            reference = null;
        }
        
        //build MPmaps
        File[] listOfYellow = outputfolder2.listFiles() ;
        File[] listOfBlue = outputfolder3.listFiles() ;
        float bMax, bMin, yMax = 0, yMin = 0;
        int counter = 1;
        ModelImage yellow = null, blue = null, mpMap = null;
        float[] yBuffer = null, bBuffer, newBuffer;
        for (int i = 0; i < listOfYellow.length; i ++){ //yellow images
            yellow = fileIO.readImage(listOfYellow[i].getAbsoluteFile().toString());
            eyebitset = eyeballSelect(yellow, eyebitset, -1); //set for finding min/max
            int length = yellow.getExtents(0)[0] * yellow.getExtents(0)[1];
            yBuffer =  new float[length];
            
            try {
                yellow.exportData(0, length, yBuffer);
            }
            catch (IOException error) {
                System.out.println("IO exception");
                return;
            }
            yMax = findTrueMax(yBuffer); 
            yMin = findTrueMin(yBuffer);
            

            
            
            for ( int j = 0; j < listOfBlue.length; j++){ //blue images
                blue = fileIO.readImage(listOfBlue[j].getAbsoluteFile().toString());
                int length2 = blue.getExtents(0)[0] * blue.getExtents(0)[1];
                bBuffer =  new float[length2];
                try {
                    blue.exportData(0, length2, bBuffer);
                }
                catch (IOException error) {
                    System.out.println("IO exception");
                    return;
                }
                eyebitset = eyeballSelect(blue, eyebitset, -1); //set for finding min/max
                bMax = findTrueMax(bBuffer);
                bMin = findTrueMin(bBuffer);
                outputbox.append("**Creating MPmap " + counter + " of " + listOfBlue.length * listOfYellow.length + "......");
                outputbox.setCaretPosition( outputbox.getText().length() );
                //create mpMap
                newBuffer = MPmaper(yBuffer, bBuffer, yMax, yMin, bMax, bMin);
                
                mpMap = new ModelImage(ModelStorageBase.FLOAT, blue.getExtents(), "MPMap Yellow" + i+1 + " to Blue" + j+1);
               
                try {
                    mpMap.importData(0, newBuffer, true);
                } catch (IOException e) {

                }
                
                //save MPmap
                opts.setFileDirectory(outputfolder + outputfolder.separator);
                opts.setFileName("MPMap Yellow" + (i+1) + " to Blue" + (j+1));
                fileIO.writeImage(mpMap, opts);
                outputbox.append("done \n");
                counter++;
                

                
                
                
                if (mpMap != null){
                    mpMap.disposeLocal();
                    mpMap = null;
                }
                if (blue != null){
                    blue.disposeLocal();
                    blue = null;
                }
                if (yellow != null){
                    yellow.disposeLocal();
                    yellow = null;
                }
                
                
            }
        }
        
        //find mean/st dev image
    outputbox.append("**Calculating Mean Image and St Dev......");
    outputbox.setCaretPosition( outputbox.getText().length() );    
    File[] listOfMP = outputfolder.listFiles();
    ModelImage allMPs[] = new ModelImage[listOfMP.length];
    for (int i = 0; i < listOfMP.length; i++){
        allMPs[i] = fileIO.readImage(listOfMP[i].getAbsoluteFile().toString());
    }
    
    //save them
    ModelImage stDevImg = stDev(allMPs);
    opts.setFileName("MPMap Average");
    fileIO.writeImage(avg, opts);
    opts.setFileName("MPMap StDev");
    fileIO.writeImage(stDevImg, opts);
    
    outputbox.append("done \n");
    outputbox.setCaretPosition( outputbox.getText().length() );  
    
    
    if (stDevImg != null){
        stDevImg.disposeLocal();
        stDevImg = null;
    }
    
        
    for (int i = 0; i < allMPs.length; i++){ //close images
        if (allMPs[i] != null){
            allMPs[i].disposeLocal();
            allMPs[i] = null;
        }
    }
    
    
    
    
    
        
    outputbox.append("** Ending Algorithm v0.5 **\n");
    outputbox.setCaretPosition( outputbox.getText().length() );    
    setCompleted(true);
    return;

    }
    /** makes circle mask around the center of eye, and saves it as bitset **/
    public BitSet eyeballSelect(ModelImage eye,  BitSet bitset, float size){
        
        //finds center of eye and puts a mask of certain size

        AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(eye, 1.0f, 1.0f);

        regionGrowAlgo.setRunningInSeparateThread(false);

        
        int lpix = 0, rpix = 0;
        int Midy = (eye.getExtents()[1])/2;
        int xDim = eye.getExtents()[0];


        
        for (int i = 0, maxpixl = 0 ; maxpixl< 1000; i++){
            maxpixl = eye.get(i, Midy).intValue();
            lpix = i;
        }
        for (int j = xDim, maxpixr = 0 ; maxpixr < 1000; j--){
            maxpixr = eye.get(j, Midy).intValue();
            rpix = j;
        }
        
        int centerX = (rpix + lpix)/2;



       regionGrowAlgo.regionGrow2D(bitset, new Point(centerX, Midy), -1,false, false, null, 850,3500, -1, size, false);


        return bitset;
    }
    
    /** uses gradient magnitude to register match to ref **/
    public ModelImage registration(ModelImage ref, ModelImage match){
        
        ModelImage temp = (ModelImage) match.clone();
        //convert to GM for registration
        outputbox.append("        ** Calculating Gradient Magnitude for Registration **\n");
        outputbox.setCaretPosition( outputbox.getText().length() );
        AlgorithmGradientMagnitudeSep algogMag = new AlgorithmGradientMagnitudeSep(temp, sigma, true, false);
        algogMag.run();
        try{
            temp.importData(0, algogMag.getResultBuffer(), true);
        }catch(IOException e){
            
        }
        
        //registers Match to Reg using refeyebitset as weight


        float[] refRes = new float[] {
                       ref.getFileInfo(0).getResolutions()[0],
                       ref.getFileInfo(0).getResolutions()[1]
                    };
       float[] matchRes = new float[] {
                         match.getFileInfo(0).getResolutions()[0],
                          match.getFileInfo(0).getResolutions()[1]
                      };
        
        ModelImage refWeightImage = new ModelImage(1, ref.getExtents(), "VOI ref");
        ModelImage inputWeightImage = new ModelImage(1, match.getExtents(), "VOI match");
        
        refWeightImage.getFileInfo(0).setResolutions(refRes);
        inputWeightImage.getFileInfo(0).setResolutions(matchRes);
        
        // make new reference and input images based on the VOIs in them.
        // pass those new images to the registration algorithm
        BitSet mask = eyebitset;
        int imageSize = ref.getSliceSize();
        
        for (int i = 0; i < imageSize; i++) {
        
        if (!mask.get(i)) {
           refWeightImage.set(i, 0);
        } else {
           refWeightImage.set(i, 1);
        }
        }
        imageSize = match.getSliceSize();
        
        for (int i = 0; i < imageSize; i++) {
        
        if (!mask.get(i)) {
           inputWeightImage.set(i, 0);
        } else {
           inputWeightImage.set(i, 1);
        }
        }
        outputbox.append("        ** Registering to Reference Image **\n");
        outputbox.setCaretPosition( outputbox.getText().length() );
        AlgorithmRegOAR2D reg2 = new AlgorithmRegOAR2D(ref, temp, refWeightImage, inputWeightImage, 0, 3, 1,(float) -10, (float) 10,(float)  3, (float) 2, true, 10,2, 3);
        reg2.run();
        
        refWeightImage.disposeLocal();
        refWeightImage = null;
        inputWeightImage.disposeLocal();
        inputWeightImage = null;
        
        if (temp != null){
            temp.disposeLocal();
            temp = null;
        }
        //get cost
        currCost = reg2.getCost();

        float xresA = ref.getFileInfo(0).getResolutions()[0];
        float yresA = ref.getFileInfo(0).getResolutions()[1];

        //transform non gradient image w/ matrix
        AlgorithmTransform transform = new AlgorithmTransform(match, reg2.getTransform(), 1, xresA, yresA, match.getExtents()[0],
            match.getExtents()[1], true, false, false);

        
        
        transform.setUpdateOriginFlag(true);
        transform.run();

       
        match = transform.getTransformedImage();
        if (reg2 != null){
            reg2.disposeLocal();
            reg2 = null;
        }
   
        if (transform != null){
            transform.disposeLocal();
            transform = null;
        }
        

        match.calcMinMax();

        if (transform != null){
            transform.disposeLocal();
            transform = null;
        }
        
        //returns new image
        return match;
    }

    /** Finds Min while ignoring "outliers" **/
    public float findTrueMin(float[] buffer)
    {
       float min = 10000;
       for (int i = 0; i < buffer.length; i++){
           if (buffer[i] < min && eyebitset.get(i) && buffer[i] > 100)
              min = buffer[i];
       }
       System.out.println(min);
       return min;
    }
    /** Finds Max while ignoring "outliers" **/   
    public float findTrueMax(float[] buffer)
    {
       float max = 0;
       for (int i = 0; i < buffer.length; i++){
           if (buffer[i] > max && eyebitset.get(i))
              max = buffer[i];
       }
       System.out.println(max);
       return max;
    }
    
    /** generates MPmap by using supplied formula **/
    public float[] MPmaper(float[] yBuffer, float[] bBuffer, float yMax, float yMin, float bMax, float bMin){
        float[] newBuffer = new float[yBuffer.length];
        
        for (int i = 0; i < newBuffer.length; i++){
            if ((epsB == epsY) || (bBuffer[i] == bMin) || (yBuffer[i] == yMin)){//remove outlieing pixels
                newBuffer[i] = 0;
            }
            else{
            newBuffer[i] = (1/(epsB - epsY)) * (float)(Math.log(((double)bMax - (double)bMin) / ((double)bBuffer[i] - (double)bMin)) - Math.log(((double)yMax - (double)yMin) / ((double)yBuffer[i] - (double)yMin)));
            
            }
            if (newBuffer[i] < -100 || newBuffer[i] > 100){//remove outlieing pixels
                newBuffer[i] = 0;
            }
                
                         
        }
        
        
        return newBuffer;
    }
    
    /** generates mean image while finding and saving St Dev image **/
    public ModelImage stDev(ModelImage[] imgs){
        ModelImage avgImg = new ModelImage(ModelStorageBase.FLOAT, imgs[0].getExtents(), "Average");
        int length = imgs[0].getSize();
        float mean[] = new float[length], current[]= new float[length], total[]= new float[length];
        //find average image
        AlgorithmImageCalculator algoAvg = new AlgorithmImageCalculator(avgImg, imgs, AlgorithmImageCalculator.AVERAGE, 0);
        algoAvg.runAlgorithm();
        avg = (ModelImage) avgImg.clone();
        
        //now generate st dev image
        try {
            avgImg.exportData(0, length, mean);
        } catch (IOException e) {
            
        }
        for(int i = 0; i < mean.length; i++){ //remove outlieing pixels
            if (mean[i] < -100 || mean[i] > 100)
                mean[i] = 0;
        }
        try {
            avg.importData(0, mean, true);
        } catch (IOException e) {
        }
        
        
        for (int i = 0; i < imgs.length; i++){
            try {
                imgs[i].exportData(0, length, current);
            } catch (IOException e) {
                
            }
            
            for (int j = 0; j < mean.length; j++){
                current[j] = current[j] - mean[j];
                current[j] = current[j] * current[j];
                total[j] = current[j] + total[j];
            }
        }
        
        for (int i = 0; i < total.length; i++){
            total[i] = total[i]/imgs.length;
            total[i] = (float) Math.sqrt(total[i]);
            if (total[i] < -100 || total[i] > 100){
                total[i] = 0;
            }
        }
        
        
        ModelImage stDev = new ModelImage(ModelStorageBase.FLOAT, avgImg.getExtents(), "St Dev");
        try {
            stDev.importData(0, total, true);
        } catch (IOException e) {
        }
            
        if (avgImg != null){
            avgImg.disposeLocal();
            avgImg = null;
        }
        return stDev;
            
        
        
        
    }
}
