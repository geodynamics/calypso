!> @file  t_MHD_file_parameter.f90
!!      module t_MHD_file_parameter
!!
!! @author  H. Matsui
!! @date Programmed in June, 2013
!
!> @brief Structure for file information (prefix, format, IO flag)
!!
!!@verbatim
!!@endverbatim
!
      module t_MHD_file_parameter
!
      use m_precision
      use t_file_IO_parameter
!
      implicit none
!
!
!>      Structure of file name and format for MHD
      type MHD_file_IO_params
!>        Structure of mesh file IO paramters
        type(field_IO_params) :: mesh_file_IO
!>        Structure of file name and format for restart file
        type(field_IO_params) :: fst_file_IO
!
!>        Structure of file name and format for model coefficients
        type(field_IO_params) :: Csim_file_IO
!>        Structure of file name and format for commutation coefficients
        type(field_IO_params) :: Cdiff_file_IO
!
!>        Structure of file name and format for restart file
        type(field_IO_params) :: ucd_file_IO
!>        Structure of file name and format for spectr data file
        type(field_IO_params) :: sph_file_IO
!
!>        Structure of old spherical shell mesh file
        type(field_IO_params) :: org_rj_file_IO
!>        Structure for original restart file  paramters
        type(field_IO_params) :: org_rst_file_IO
!>        Structure for original restart file  paramters
        type(field_IO_params) :: org_ucd_file_IO
      end type MHD_file_IO_params
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine set_control_org_sph_files(org_plt, MHD_files)
!
      use m_default_file_prefix
      use set_control_platform_data
!
      type(platform_data_control), intent(in) :: org_plt
      type(MHD_file_IO_params), intent(inout) :: MHD_files
!
!
      call set_control_mesh_file_def                                    &
     &   (def_org_sph_rj_head, org_plt, MHD_files%org_rj_file_IO)
      call set_control_mesh_file_def                                    &
     &   (def_org_rst_header, org_plt, MHD_files%org_rst_file_IO)
!
      end subroutine set_control_org_sph_files
!
!------------------------------------------------------------------
!
      end module t_MHD_file_parameter
