!>@file  t_work_SPH_MHD.f90
!!       module t_work_SPH_MHD
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Work structures for spherical shell dynamo
!!
!!@verbatim
!!@endverbatim
      module t_work_SPH_MHD
!
      use m_precision
      use m_constants
!
      use t_fdm_coefs
      use t_radial_matrices_sph_MHD
      use t_work_4_sph_trans
      use t_sph_trans_arrays_MHD
      use t_sph_mhd_monitor_data_IO
!
      implicit  none
!
!
!>        Structures of work area for spherical shell dynamo
      type work_SPH_MHD
!>        Structure of 2nd order FDM matrices
        type(fdm_matrices) :: r_2nd
!>          Structure of 4th order FDM matrices
        type(fdm_matrices) :: r_4th
!>        Structure of 1st order FDM matrices on element
        type(fdm_matrices) :: r_2nd_ele
!
!>        Structure of band matrices for dynamo simulation
        type(MHD_radial_matrices) :: MHD_mats
!
        type(parameters_4_sph_trans) :: trans_p
!
!>        strucutres for spherical transform dor MHD dynamo
        type(works_4_sph_trans_MHD) :: trns_WK
!
!
        type(sph_mhd_monitor_data) :: monitor
      end type work_SPH_MHD
!
      end module t_work_SPH_MHD
