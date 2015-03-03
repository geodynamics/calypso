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
      use t_read_control_arrays
!
      implicit  none
!
!
!>      Structure for number and power to construct
!!               evolution of magnetic field term
!!@n      coef_4_magne_evo_ctl%c_tbl:  Name of number 
!!@n      coef_4_magne_evo_ctl%vect:   Power of the number
      type(ctl_array_cr), save :: coef_4_magne_evo_ctl
!
!>      Structure for number and power to construct
!!               magnetic diffusion term
!!@n      coef_4_mag_diffuse_ctl%c_tbl:  Name of number 
!!@n      coef_4_mag_diffuse_ctl%vect:   Power of the number
      type(ctl_array_cr), save :: coef_4_mag_diffuse_ctl
!
!>      Structure for number and power to construct
!!               gradient of potenrial
!!@n      coef_4_mag_potential_ctl%c_tbl:  Name of number 
!!@n      coef_4_mag_potential_ctl%vect:   Power of the number
      type(ctl_array_cr), save :: coef_4_mag_potential_ctl
!
!>      Structure for number and power to construct magnetic induction
!!@n      coef_4_induction_ctl%c_tbl:  Name of number 
!!@n      coef_4_induction_ctl%vect:   Power of the number
      type(ctl_array_cr), save :: coef_4_induction_ctl
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
      private :: hd_induction, i_induct_ctl
      private :: hd_n_magne, hd_n_mag_p
      private :: hd_n_m_diff, hd_n_induct
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_magne_ctl
!
      call dealloc_control_array_c_r(coef_4_magne_evo_ctl)
!
      end subroutine deallocate_coef_4_magne_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_mag_p_ctl
!
      call dealloc_control_array_c_r(coef_4_mag_potential_ctl)
!
      end subroutine deallocate_coef_4_mag_p_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_m_diffuse_ctl
!
      call dealloc_control_array_c_r(coef_4_mag_diffuse_ctl)
!
      end subroutine deallocate_coef_4_m_diffuse_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_coef_4_induction_ctl
!
      call dealloc_control_array_c_r(coef_4_induction_ctl)
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
        call read_control_array_c_r(hd_n_magne, coef_4_magne_evo_ctl)
        call read_control_array_c_r                                     &
     &     (hd_n_mag_p, coef_4_mag_potential_ctl)
        call read_control_array_c_r                                     &
     &     (hd_n_m_diff, coef_4_mag_diffuse_ctl)
        call read_control_array_c_r(hd_n_induct, coef_4_induction_ctl)
      end do
!
      end subroutine read_induction_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_induct_norm
