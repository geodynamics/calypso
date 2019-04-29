!>@file   m_elapsed_labels_4_MHD.f90
!!@brief  module m_elapsed_labels_4_MHD
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2018
!
!>@brief  Labels for elapsed time monitor
!!
!!@verbatim
!!      subroutine elapsed_label_4_MHD
!!      subroutine elapsed_label_4_SPH_MHD
!!      subroutine elapsed_label_4_FEM_MHD
!!      subroutine elapsed_label_4_SGS_model
!!      subroutine elapsed_label_4_Legendre_trans
!!
!!      subroutine reset_elapse_after_init_SPH_MHD
!!      subroutine reset_elapse_after_init_SGS
!!      subroutine reset_elapse_after_init_LEG
!!@endverbatim
!!
      module m_elapsed_labels_4_MHD
!
      use m_precision
      use m_work_time
!
      implicit none
!
!
      logical, save :: iflag_MHD_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_MHD =  0
      integer(kind = kint), save :: ied_elapsed_MHD =  0
!
      logical, save :: iflag_SMHD_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_SMHD =  0
      integer(kind = kint), save :: ied_elapsed_SMHD =  0
!
      logical, save :: iflag_FMHD_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_FMHD =  0
      integer(kind = kint), save :: ied_elapsed_FMHD =  0
!
      logical, save :: iflag_SGS_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_SGS =  0
      integer(kind = kint), save :: ied_elapsed_SGS =  0
!
      logical, save :: iflag_LEG_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_LEG =   0
      integer(kind = kint), save :: ied_elapsed_LEG =   0
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine elapsed_label_4_MHD
!
      integer(kind = kint), parameter :: num_append = 4
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_MHD, ied_elapsed_MHD)
!
      elps1%labels(ist_elapsed_MHD+1) = 'Initialization time        '
      elps1%labels(ist_elapsed_MHD+2) = 'Time evolution loop time   '
      elps1%labels(ist_elapsed_MHD+3) = 'Data IO time               '
      elps1%labels(ist_elapsed_MHD+4) = 'Visualizatio time          '
!
      iflag_MHD_time = .TRUE.
!
      end subroutine elapsed_label_4_MHD
!
!-----------------------------------------------------------------------
!
      subroutine elapsed_label_4_SPH_MHD
!
      integer(kind = kint), parameter :: num_append = 12
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_SMHD, ied_elapsed_SMHD)
!
      elps1%labels(ist_elapsed_SMHD+ 1) = 'Evolution excluding IO    '
      elps1%labels(ist_elapsed_SMHD+ 2) = 'Linear time               '
      elps1%labels(ist_elapsed_SMHD+ 3) = 'Solver time               '
      elps1%labels(ist_elapsed_SMHD+ 4) = 'Nonlinear terms           '
!
      elps1%labels(ist_elapsed_SMHD+ 5) = 'Obtain field to output    '
      elps1%labels(ist_elapsed_SMHD+ 6) = 'output_sph_restart_control'
      elps1%labels(ist_elapsed_SMHD+ 7) = 'output_rms_sph_mhd_control'
!
      elps1%labels(ist_elapsed_SMHD+ 8) = 'Coriolis term             '
      elps1%labels(ist_elapsed_SMHD+ 9) = 'sph backward transform    '
      elps1%labels(ist_elapsed_SMHD+10) = 'cal nonlinear terms       '
      elps1%labels(ist_elapsed_SMHD+11) = 'sph forward transform     '
      elps1%labels(ist_elapsed_SMHD+12) = 'obtain explicit terms     '
!
      iflag_SMHD_time = .TRUE.
!
      end subroutine elapsed_label_4_SPH_MHD
!
!-----------------------------------------------------------------------
!
      subroutine elapsed_label_4_FEM_MHD
!
      integer(kind = kint), parameter :: num_append = 1
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_FMHD, ied_elapsed_FMHD)
!
      elps1%labels(ist_elapsed_FMHD+ 1) = 'Linear solver time '
!
      iflag_FMHD_time = .TRUE.
!
      end subroutine elapsed_label_4_FEM_MHD
!
!-----------------------------------------------------------------------
!
      subroutine elapsed_label_4_SGS_model
!
      integer(kind = kint), parameter :: num_append = 4
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_SGS, ied_elapsed_SGS)
!
      elps1%labels(ist_elapsed_SGS+1) = 'Filtering fields   '
      elps1%labels(ist_elapsed_SGS+2) = 'Scale similarity   '
      elps1%labels(ist_elapsed_SGS+3) = 'Dynamic scheme     '
      elps1%labels(ist_elapsed_SGS+4) = 'SGS Buoyancy       '
!
      iflag_SGS_time = .TRUE.
!
      end subroutine elapsed_label_4_SGS_model
!
!-----------------------------------------------------------------------
!
      subroutine elapsed_label_4_Legendre_trans
!
      integer(kind = kint), parameter :: num_append = 10
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_LEG, ied_elapsed_LEG)
!
      elps1%labels(ist_elapsed_LEG+1) = 'Copy P_lm for bwd. trans.   '
      elps1%labels(ist_elapsed_LEG+2)                                   &
     &         = 'Copy spectrum for bwd. trans.  '
      elps1%labels(ist_elapsed_LEG+3)                                   &
     &         = 'mat product for bwd. trans.    '
      elps1%labels(ist_elapsed_LEG+4)                                   &
     &         = 'Copy fields to bwd. trans.     '
      elps1%labels(ist_elapsed_LEG+5)                                   &
     &         = 'Equator for bwd. trans.        '
      elps1%labels(ist_elapsed_LEG+6)                                   &
     &         = 'Copy P_lm for fwd. trans.      '
      elps1%labels(ist_elapsed_LEG+7)                                   &
     &         = 'Copy field for fwd. trans.     '
      elps1%labels(ist_elapsed_LEG+8)                                   &
     &         = 'mat product for fwd. trans.    '
      elps1%labels(ist_elapsed_LEG+9)                                   &
     &         = 'Copy spectrum to fwd. trans.   '
!
      elps1%labels(ist_elapsed_LEG+10)                                  &
     &         = 'mhd_spectr_to_sendbuf.   '
!
      iflag_LEG_time = .TRUE.
!
      end subroutine elapsed_label_4_Legendre_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_SPH_MHD
!
!
      if(iflag_SMHD_time .eqv. .FALSE.) return
      call reset_elapsed_times(ist_elapsed_SMHD+1, ied_elapsed_SMHD)
!
      end subroutine reset_elapse_after_init_SPH_MHD
!
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_SGS
!
!
      if(iflag_SGS_time .eqv. .FALSE.) return
      call reset_elapsed_times(ist_elapsed_SGS+1, ied_elapsed_SGS)
!
      end subroutine reset_elapse_after_init_SGS
!
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_LEG
!
!
      if(iflag_LEG_time .eqv. .FALSE.) return
      call reset_elapsed_times(ist_elapsed_LEG+1, ist_elapsed_LEG)
!
      end subroutine reset_elapse_after_init_LEG
!
!-----------------------------------------------------------------------
!
      end module m_elapsed_labels_4_MHD
