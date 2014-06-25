!>@file   set_phys_name_4_sph_trans.f90
!!@brief  module set_phys_name_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2008
!
!!@verbatim
!!      subroutine copy_sph_name_rj_to_rtp
!!@endverbatim
!
      module set_phys_name_4_sph_trans
!
      use m_precision
!
      implicit  none
! 
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_sph_name_rj_to_rtp
!
      use m_constants
      use m_machine_parameter
      use m_phys_constants
      use m_sph_spectr_data
      use m_work_4_sph_trans
!
      integer(kind = kint) :: i, i0
!
      num_phys_rtp =  num_phys_rj
      call allocate_phys_rtp_name
!
      i0 = 0
      num_scalar_rtp = 0
      istack_phys_comp_rtp(0) = 0
      do i = 1, num_phys_rj
        if (num_phys_comp_rj(i) .eq. n_scalar) then
          i0 = i0 + 1
          num_scalar_rtp = num_scalar_rtp + 1
          num_phys_comp_rtp(i0) =    num_phys_comp_rj(i)
          istack_phys_comp_rtp(i0) = istack_phys_comp_rtp(i0-1)         &
     &                              + num_phys_comp_rtp(i0)
          phys_name_rtp(i0) =        phys_name_rj(i)
          iflag_monitor_rtp(i0) =    iflag_monitor_rj(i)
        end if
      end do
      istart_scalar_rtp = 1
!
      num_vector_rtp = 0
      do i = 1, num_phys_rj
        if (num_phys_comp_rj(i) .eq. n_vector) then
          i0 = i0 + 1
          num_vector_rtp = num_vector_rtp + 1
          num_phys_comp_rtp(i0) =    num_phys_comp_rj(i)
          istack_phys_comp_rtp(i0) = istack_phys_comp_rtp(i0-1)         &
     &                              + num_phys_comp_rtp(i0)
          phys_name_rtp(i0) =        phys_name_rj(i)
          iflag_monitor_rtp(i0) =    iflag_monitor_rj(i)
        end if
      end do
      istart_vector_rtp = istart_scalar_rtp + num_scalar_rtp
!
      num_tensor_rtp = 0
      do i = 1, num_phys_rj
        if (num_phys_comp_rj(i) .eq. n_sym_tensor) then
          i0 = i0 + 1
          num_tensor_rtp = num_tensor_rtp + 1
          num_phys_comp_rtp(i0) =    num_phys_comp_rj(i)
          istack_phys_comp_rtp(i0) = istack_phys_comp_rtp(i0-1)         &
     &                              + num_phys_comp_rtp(i0)
          phys_name_rtp(i0) =        phys_name_rj(i)
          iflag_monitor_rtp(i0) =    iflag_monitor_rj(i)
        end if
      end do
      istart_tensor_rtp = istart_vector_rtp + num_vector_rtp
!
      ntot_phys_rtp = istack_phys_comp_rtp(i0)
!
      if (iflag_debug .gt. 0) then
!        write(*,*) 'num_phys_rj', num_phys_rj
!        write(*,*) 'id, components, stack, phys_name_rj'
!        do i = 1, num_phys_rj
!          write(*,*) i, num_phys_comp_rj(i), istack_phys_comp_rj(i),   &
!     &              trim(phys_name_rj(i))
!        end do
        write(*,*)
        write(*,*) 'num_phys_rtp', num_phys_rtp
        write(*,*) 'ntot_phys_rtp', ntot_phys_rtp
        write(*,*) 'phys_name_rtp', size(phys_name_rtp)
        write(*,*) 'iflag_monitor_rtp', size(iflag_monitor_rtp)
        write(*,*) 'num_phys_comp_rtp', size(num_phys_comp_rtp)
        write(*,*) 'istack_phys_comp_rtp', size(istack_phys_comp_rtp)
        write(*,*) 'id, components, stack, phys_name_rtp'
        do i = 1, num_phys_rtp
          write(*,*) i, num_phys_comp_rtp(i), istack_phys_comp_rtp(i),  &
     &              trim(phys_name_rtp(i))
        end do
        write(*,*) 'istart_scalar_rtp',                                 &
     &              istart_scalar_rtp, num_scalar_rtp
        write(*,*) 'istart_vector_rtp',                                 &
     &              istart_vector_rtp, num_vector_rtp
        write(*,*) 'istart_tensor_rtp',                                 &
     &              istart_tensor_rtp, num_tensor_rtp
      end if
!
!
      end subroutine copy_sph_name_rj_to_rtp
!
! -------------------------------------------------------------------
!
      end module set_phys_name_4_sph_trans
 