package gov.nih.mipav.model.algorithms.itk;

import InsightToolkit.*;

import gov.nih.mipav.model.structures.*;


public class PItkImage3
{
    /** Contains the itk smart pointer, which will delete our image data
     * unless we keep it around. The only common base type is Object. */
    private Object m_SmartPointer = null;
    /** 3D itk image, specialized for data type. */
    private itkImageBase3 m_itkImageBase3 = null;

    protected void finalize() {
        m_itkImageBase3 = null;
        // avoid 'local variable never read' warning. Harmless.
        if (m_SmartPointer != null) {
        	m_SmartPointer = null;
        }
    }

    /** Create instance of itkImage, matching Mipav image type, if possible.
     * Wrapper around itk smart pointer and the more useful image pointer. 
     * @param model_image_type as defined by consts in ModelStorageBase 
     */
    public PItkImage3(int model_image_type) {
        switch (model_image_type) {
        case ModelImage.BYTE:
        {
            itkImageSC3_Pointer kImageITK_p = itkImageSC3.itkImageSC3_New();
            m_SmartPointer = kImageITK_p;
            m_itkImageBase3 = kImageITK_p.GetPointer();
            break;
        }
        case ModelImage.UBYTE:
        case ModelImage.ARGB:
        {
            itkImageUC3_Pointer kImageITK_p = itkImageUC3.itkImageUC3_New();
            m_SmartPointer = kImageITK_p;
            m_itkImageBase3 = kImageITK_p.GetPointer();
            break;
        }
        case ModelImage.SHORT:
        {
            itkImageSS3_Pointer kImageITK_p = itkImageSS3.itkImageSS3_New();
            m_SmartPointer = kImageITK_p;
            m_itkImageBase3 = kImageITK_p.GetPointer();
            break;
        }
        case ModelImage.USHORT:
        case ModelImage.ARGB_USHORT:
        {
            itkImageUS3_Pointer kImageITK_p = itkImageUS3.itkImageUS3_New();
            m_SmartPointer = kImageITK_p;
            m_itkImageBase3 = kImageITK_p.GetPointer();
            break;
        }
        case ModelImage.INTEGER:
        {
            itkImageSI3_Pointer kImageITK_p = itkImageSI3.itkImageSI3_New();
            m_SmartPointer = kImageITK_p;
            m_itkImageBase3 = kImageITK_p.GetPointer();
            break;
        }
        case ModelImage.UINTEGER:
        {
            itkImageUI3_Pointer kImageITK_p = itkImageUI3.itkImageUI3_New();
            m_SmartPointer = kImageITK_p;
            m_itkImageBase3 = kImageITK_p.GetPointer();
            break;
        }
        case ModelImage.LONG:
        {
            // Possible signed/unsigned mismatch here. SL3 is not avail in Itk.
            itkImageUL3_Pointer kImageITK_p = itkImageUL3.itkImageUL3_New();
            m_SmartPointer = kImageITK_p;
            m_itkImageBase3 = kImageITK_p.GetPointer();
            break;
        }
        case ModelImage.FLOAT:
        case ModelImage.ARGB_FLOAT:
        {
            // Create instance of ITK 3D image of floating point values
            itkImageF3_Pointer kImageITK_p = itkImageF3.itkImageF3_New();
            m_SmartPointer = kImageITK_p;
            m_itkImageBase3 = kImageITK_p.GetPointer();
            break;
        }
        case ModelImage.DOUBLE:
        {
            // Create instance of ITK 3D image of floating point values
            itkImageD3_Pointer kImageITK_p = itkImageD3.itkImageD3_New();
            m_SmartPointer = kImageITK_p;
            m_itkImageBase3 = kImageITK_p.GetPointer();
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
    public PItkImage3(Object smart_pointer) {
        m_itkImageBase3 = (itkImageBase3)AutoItkLoader.invokeMethod("GetPointer", smart_pointer);
        if (m_itkImageBase3 != null) {
            m_SmartPointer = smart_pointer;
        }
    }

    /** Access the 3D image 
     * @return 3D Itk image. null if Mipav type doesn't have corresponding itk type.
     */
    public itkImageBase3 img() {
        return m_itkImageBase3;
    }

}
