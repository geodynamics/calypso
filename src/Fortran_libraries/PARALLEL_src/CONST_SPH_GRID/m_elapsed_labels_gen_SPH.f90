!>@file   m_elapsed_labels_gen_SPH.f90
!!@brief  module m_elapsed_labels_gen_SPH
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2018
!
!>@brief  Labels for elapsed time monitor
!!
!!@verbatim
!!      subroutine elpsed_label_gen_sph_grid
!!@endverbatim
!!
      module m_elapsed_labels_gen_SPH
!
      use m_precision
      use m_work_time
!
      implicit none
!
      logical, save :: iflag_GSP_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_GSP =  0
      integer(kind = kint), save :: ied_elapsed_GSP =  0
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine elpsed_label_gen_sph_grid
!
      integer(kind = kint), parameter :: num_append = 4
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_GSP, ied_elapsed_GSP)
!
      elps1%labels(ist_elapsed_GSP+1)                                   &
     &      = 'Generation of spherical transform table'
      elps1%labels(ist_elapsed_GSP+2)                                   &
     &      = 'Generation of spherical mode and grid'
      elps1%labels(ist_elapsed_GSP+3)                                   &
     &      = 'Generation of FEM mesh data'
      elps1%labels(ist_elapsed_GSP+4)                                   &
     &      = 'Generation of surface FEM mesh data'
!      elps1%labels(ist_elapsed_GSP+5)                                  &
!     &      = 'Generation of viewer data'
!
      iflag_GSP_time = .TRUE.
!
      end subroutine elpsed_label_gen_sph_grid
!
!-----------------------------------------------------------------------
!
      end module m_elapsed_labels_gen_SPH
