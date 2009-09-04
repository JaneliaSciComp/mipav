package gov.nih.mipav.model.algorithms.itk;

import InsightToolkit.*;

import gov.nih.mipav.model.structures.*;

public class PItkImage2
{
    /** Contains the itk smart pointer, which will delete our image data
     * unless we keep it around. The only common base type is Object. */
    private Object m_SmartPointer = null;
    /** 2D itk image, specialized for data type. */
    private itkImageBase2 m_itkImageBase2 = null;

    protected void finalize() {
        m_itkImageBase2 = null;
        // avoid 'local variable never read' warning. Harmless.
        if (m_SmartPointer != null) {
        	m_SmartPointer = null;
        }
    }

    /** Create instance of itkImage, matching Mipav image type, if possible.
     * Wrapper around itk smart pointer and the more useful image pointer. 
     * @param model_image_type as defined by consts in ModelStorageBase 
     */
    public PItkImage2(int model_image_type) {
        switch (model_image_type) {
        case ModelImage.BYTE:
        {
            itkImageSC2_Pointer kImageITK_p = itkImageSC2.itkImageSC2_New();
            m_SmartPointer = kImageITK_p;
            m_itkImageBase2 = kImageITK_p.GetPointer();
            break;
        }
        case ModelImage.UBYTE:
        case ModelImage.ARGB:
        {
            itkImageUC2_Pointer kImageITK_p = itkImageUC2.itkImageUC2_New();
            m_SmartPointer = kImageITK_p;
            m_itkImageBase2 = kImageITK_p.GetPointer();
            break;
        }
        case ModelImage.SHORT:
        {
            itkImageSS2_Pointer kImageITK_p = itkImageSS2.itkImageSS2_New();
            m_SmartPointer = kImageITK_p;
            m_itkImageBase2 = kImageITK_p.GetPointer();
            break;
        }
        case ModelImage.USHORT:
        case ModelImage.ARGB_USHORT:
        {
            itkImageUS2_Pointer kImageITK_p = itkImageUS2.itkImageUS2_New();
            m_SmartPointer = kImageITK_p;
            m_itkImageBase2 = kImageITK_p.GetPointer();
            break;
        }
        case ModelImage.INTEGER:
        {
            itkImageSI2_Pointer kImageITK_p = itkImageSI2.itkImageSI2_New();
            m_SmartPointer = kImageITK_p;
            m_itkImageBase2 = kImageITK_p.GetPointer();
            break;
        }
        case ModelImage.UINTEGER:
        {
            itkImageUI2_Pointer kImageITK_p = itkImageUI2.itkImageUI2_New();
            m_SmartPointer = kImageITK_p;
            m_itkImageBase2 = kImageITK_p.GetPointer();
            break;
        }
        case ModelImage.LONG:
        {
            // Possible signed/unsigned mismatch here. SL2 is not avail in Itk.
            itkImageUL2_Pointer kImageITK_p = itkImageUL2.itkImageUL2_New();
            m_SmartPointer = kImageITK_p;
            m_itkImageBase2 = kImageITK_p.GetPointer();
            break;
        }
        case ModelImage.FLOAT:
        case ModelImage.ARGB_FLOAT:
        {
            // Create instance of ITK 2D image of floating point values
            itkImageF2_Pointer kImageITK_p = itkImageF2.itkImageF2_New();
            m_SmartPointer = kImageITK_p;
            m_itkImageBase2 = kImageITK_p.GetPointer();
            break;
        }
        case ModelImage.DOUBLE:
        {
            // Create instance of ITK 2D image of floating point values
            itkImageD2_Pointer kImageITK_p = itkImageD2.itkImageD2_New();
            m_SmartPointer = kImageITK_p;
            m_itkImageBase2 = kImageITK_p.GetPointer();
            break;
        }
        case ModelImage.BOOLEAN:
        case ModelImage.COMPLEX:
        case ModelImage.DCOMPLEX:
            break;
        default:
            assert(false);
            break;
        }
        
    }

    /** Create wrapper of existing itkImage.
     * Wrapper around itk smart pointer and the more useful image pointer. 
     * @param smart_pointer existing Itk smart pointer to Itk image.
     */
    public PItkImage2(Object smart_pointer) {
        m_itkImageBase2 = (itkImageBase2)AutoItkLoader.invokeMethod("GetPointer", smart_pointer);
        if (m_itkImageBase2 != null) {
            m_SmartPointer = smart_pointer;
        }
    }

    /** Access the 2D image 
     * @return 2D Itk image.  null if Mipav type doesn't have corresponding itk type.
     */
    public itkImageBase2 img() {
        return m_itkImageBase2;
    }

}
