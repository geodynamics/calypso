!m_ctl_data_induct_norm.f90
!      module m_ctl_data_induct_norm
!
!        programmed by H.Matsui on March. 2006
!      subroutine deallocate_coef_4_magne_ctl
!      subroutine deallocate_coef_4_mag_p_ctl
!      subroutine deallocate_coef_4_m_diffuse_ctl
!      subroutine deallocate_coef_4_induction_ctl
!
!      subroutine read_induction_ctl
!
!   --------------------------------------------------------------------
! example
!      begin induction
!        array coef_4_magnetic_ctl   1
!          coef_4_magnetic_ctl       One                        1.0  end
!        end array
!        array coef_4_mag_p_ctl      1
!          coef_4_mag_p_ctl          One                        1.0  end
!        end array
!        array coef_4_m_diffuse_ctl  1
!          coef_4_m_diffuse_ctl      magnetic_Prandtl_number   -1.0  end
!        end array
!        array coef_4_induction_ctl  1
!          coef_4_induction_ctl      One                       -1.0  end
!        end array
!      end  induction
!   --------------------------------------------------------------------
!
      module m_ctl_data_induct_norm
!
      use m_precision
!
      implicit  none
!
!
      integer(kind=kint) :: num_coef_4_m_diffuse_ctl = 0
      character(len=kchara),allocatable :: coef_4_m_diffuse_name_ctl(:)
      real (kind = kreal), allocatable :: coef_4_m_diffuse_power_ctl(:)
!
      integer(kind=kint) :: num_coef_4_magnetic_ctl =  0
      integer(kind=kint) :: num_coef_4_mag_p_ctl =     0
      character(len=kchara),allocatable :: coef_4_magnetic_name_ctl(:)
      character(len=kchara),allocatable :: coef_4_mag_p_name_ctl(:)
      real (kind = kreal), allocatable :: coef_4_magnetic_power_ctl(:)
      real (kind = kreal), allocatable :: coef_4_mag_p_power_ctl(:)
!
      integer(kind=kint) :: num_coef_4_induction_ctl = 0
      character(len=kchara),allocatable :: coef_4_induction_name_ctl(:)
      real(kind = kreal), allocatable :: coef_4_induction_power_ctl(:)
!
!   entry label
!
      character(len=kchara), parameter :: hd_induction = 'induction'
      integer (kind=kint) :: i_induct_ctl =    0
!
!   5th level for coefs for induction
!
      character(len=kchara) :: hd_n_magne =  'coef_4_magnetic_ctl'
      character(len=kchara) :: hd_n_mag_p =  'coef_4_mag_p_ctl'
      character(len=kchara) :: hd_n_m_diff = 'coef_4_m_diffuse_ctl'
      character(len=kchara) :: hd_n_induct = 'coef_4_induction_ctl'
!
      integer (kind=kint) :: i_n_magne =  0
      integer (kind=kint) :: i_n_mag_p =  0
      integer (kind=kint) :: i_n_m_diff = 0
      integer (kind=kint) :: i_n_induct = 0
!
      private :: hd_induction, i_induct_ctl
      private :: hd_n_magne, hd_n_mag_p
      private :: hd_n_m_diff, hd_n_induct
!
      private :: allocate_coef_4_magne_ctl
      private :: allocate_coef_4_mag_p_ctl
      private :: allocate_coef_4_m_diffuse_ctl
      private :: allocate_coef_4_induction_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
       subroutine allocate_coef_4_magne_ctl
!
        allocate(coef_4_magnetic_name_ctl(num_coef_4_magnetic_ctl))
        allocate(coef_4_magnetic_power_ctl(num_coef_4_magnetic_ctl))
        coef_4_magnetic_power_ctl = 0.0d0
!
       end subroutine allocate_coef_4_magne_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_coef_4_mag_p_ctl
!
        allocate(coef_4_mag_p_name_ctl(num_coef_4_mag_p_ctl))
        allocate(coef_4_mag_p_power_ctl(num_coef_4_mag_p_ctl))
        coef_4_mag_p_power_ctl = 0.0d0
!
       end subroutine allocate_coef_4_mag_p_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_coef_4_m_diffuse_ctl
!
        allocate(coef_4_m_diffuse_name_ctl(num_coef_4_m_diffuse_ctl))
        allocate(coef_4_m_diffuse_power_ctl(num_coef_4_m_diffuse_ctl))
        coef_4_m_diffuse_power_ctl = 0.0d0
!
       end subroutine allocate_coef_4_m_diffuse_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_coef_4_induction_ctl
!
        allocate(coef_4_induction_name_ctl(num_coef_4_induction_ctl))
        allocate(coef_4_induction_power_ctl(num_coef_4_induction_ctl))
        coef_4_induction_power_ctl = 0.0d0
!
       end subroutine allocate_coef_4_induction_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine deallocate_coef_4_magne_ctl
!
        deallocate(coef_4_magnetic_name_ctl, coef_4_magnetic_power_ctl)
!
       end subroutine deallocate_coef_4_magne_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_coef_4_mag_p_ctl
!
        deallocate(coef_4_mag_p_name_ctl, coef_4_mag_p_power_ctl)
!
       end subroutine deallocate_coef_4_mag_p_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_coef_4_m_diffuse_ctl
!
        deallocate(coef_4_m_diffuse_name_ctl)
        deallocate(coef_4_m_diffuse_power_ctl)
!
       end subroutine deallocate_coef_4_m_diffuse_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_coef_4_induction_ctl
!
        deallocate(coef_4_induction_name_ctl)
        deallocate(coef_4_induction_power_ctl)
!
       end subroutine deallocate_coef_4_induction_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_induction_ctl
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_induction) .eq. 0) return
      if (i_induct_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_induction, i_induct_ctl)
        if(i_induct_ctl .gt. 0) exit
!
!
        call find_control_array_flag(hd_n_magne,                        &
     &      num_coef_4_magnetic_ctl)
        if(num_coef_4_magnetic_ctl.gt.0 .and. i_n_magne.eq.0) then
          call allocate_coef_4_magne_ctl
          call read_control_array_vect_list(hd_n_magne,                 &
     &        num_coef_4_magnetic_ctl, i_n_magne,                       &
     &        coef_4_magnetic_name_ctl, coef_4_magnetic_power_ctl)
        end if
!
!
        call find_control_array_flag(hd_n_mag_p,                        &
     &      num_coef_4_mag_p_ctl)
        if(num_coef_4_mag_p_ctl.gt.0 .and. i_n_mag_p.eq.0) then
          call allocate_coef_4_mag_p_ctl
          call read_control_array_vect_list(hd_n_mag_p,                 &
     &        num_coef_4_mag_p_ctl, i_n_mag_p,                          &
     &        coef_4_mag_p_name_ctl, coef_4_mag_p_power_ctl)
        end if
!
        call find_control_array_flag(hd_n_m_diff,                       &
     &      num_coef_4_m_diffuse_ctl)
        if(num_coef_4_m_diffuse_ctl.gt.0 .and. i_n_m_diff.eq.0) then
          call allocate_coef_4_m_diffuse_ctl
          call read_control_array_vect_list(hd_n_m_diff,                &
     &        num_coef_4_m_diffuse_ctl, i_n_m_diff,                     &
     &        coef_4_m_diffuse_name_ctl, coef_4_m_diffuse_power_ctl)
        end if
!
        call find_control_array_flag(hd_n_induct,                       &
     &      num_coef_4_induction_ctl)
        if(num_coef_4_induction_ctl.gt.0 .and. i_n_induct.eq.0) then
          call allocate_coef_4_induction_ctl
          call read_control_array_vect_list(hd_n_induct,                &
     &        num_coef_4_induction_ctl, i_n_induct,                     &
     &        coef_4_induction_name_ctl, coef_4_induction_power_ctl)
        end if
      end do
!
      end subroutine read_induction_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_induct_norm
