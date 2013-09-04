!
!      module copy_rtp_phys_data_4_IO
!
!      Written by H. Matsui on Oct., 2007
!
!      subroutine copy_rtp_phys_name_to_IO
!      subroutine copy_rtp_phys_data_to_IO
!      subroutine copy_rtp_phys_name_from_IO
!      subroutine copy_rtp_phys_data_from_IO
!
      module copy_rtp_phys_data_4_IO
!
      use m_precision
!
      use m_sph_spectr_data
      use m_spheric_parameter
      use m_field_data_IO
!
      implicit none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_rtp_phys_name_to_IO
!
      numgrid_phys_IO =   nnod_rtp
      num_phys_data_IO =  num_phys_rtp
      ntot_phys_data_IO = ntot_phys_rtp
!
      call allocate_phys_data_name_IO
      call allocate_phys_data_IO
!
      num_phys_comp_IO(1:num_phys_rtp)                                  &
     &      = num_phys_comp_rtp(1:num_phys_rtp)
      istack_phys_comp_IO(0:num_phys_rtp)                               &
     &      = istack_phys_comp_rtp(0:num_phys_rtp)
      phys_data_name_IO(1:num_phys_rtp) = phys_name_rtp(1:num_phys_rtp)
!
      end subroutine copy_rtp_phys_name_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rtp_phys_data_to_IO
!
      integer(kind = kint) :: nd
!
      do nd = 1, ntot_phys_rtp
        phys_data_IO(1:nnod_rtp,nd) = d_rtp(1:nnod_rtp,nd)
      end do
!
      end subroutine copy_rtp_phys_data_to_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_rtp_phys_name_from_IO
!
      num_phys_rtp =  num_phys_data_IO
      ntot_phys_rtp = ntot_phys_data_IO
!
      call allocate_phys_rtp_name
      call allocate_phys_rtp_data
!
      num_phys_comp_rtp(1:num_phys_rtp)                                 &
     &      = num_phys_comp_IO(1:num_phys_rtp)
      istack_phys_comp_rtp(0:num_phys_rtp)                              &
     &      = istack_phys_comp_IO(0:num_phys_rtp)
      phys_name_rtp(1:num_phys_rtp) = phys_data_name_IO(1:num_phys_rtp)
      iflag_monitor_rtp(1:num_phys_rtp) = 1
!
      end subroutine copy_rtp_phys_name_from_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rtp_phys_data_from_IO
!
      integer(kind = kint) :: nd
!
      do nd = 1, ntot_phys_rtp
        d_rtp(1:nnod_rtp,nd) = phys_data_IO(1:nnod_rtp,nd)
      end do
!
      end subroutine copy_rtp_phys_data_from_IO
!
! -------------------------------------------------------------------
!
      end module copy_rtp_phys_data_4_IO
