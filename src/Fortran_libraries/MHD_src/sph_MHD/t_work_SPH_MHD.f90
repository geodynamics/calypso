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
      use t_sph_radial_interpolate
      use t_node_monitor_IO
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
!>        Structure of 3rd order FDM matrices on element
        type(fdm_matrices) :: r_n2e_3rd
!>        Structure of 1st order FDM matrices from element
        type(fdm_matrices) :: r_e2n_1st
!
!>        Structure of band matrices for dynamo simulation
        type(MHD_radial_matrices) :: MHD_mats
!
        type(parameters_4_sph_trans) :: trans_p
!
!>        strucutres for spherical transform dor MHD dynamo
        type(works_4_sph_trans_MHD) :: trns_WK
!
!>        monitor output date from spectrum
        type(sph_mhd_monitor_data) :: monitor
!
!>        Interpolation tsble in radial direction
        type(sph_radial_interpolate) :: rj_itp
!
!>        Addresses of node monitor data
        type(node_monitor_IO) :: nod_mntr
      end type work_SPH_MHD
!
      end module t_work_SPH_MHD
