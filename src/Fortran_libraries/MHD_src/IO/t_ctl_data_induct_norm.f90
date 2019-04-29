!t_ctl_data_induct_norm.f90
!      module t_ctl_data_induct_norm
!
!        programmed by H.Matsui on March. 2006
!
!!      subroutine read_induction_ctl(hd_block, iflag, induct_ctl)
!!      subroutine bcast_induction_ctl(induct_ctl)
!!      subroutine dealloc_induction_ctl(induct_ctl)
!!        type(induction_equation_control), intent(inout) :: induct_ctl
!!
!!   --------------------------------------------------------------------
!! example
!!      begin induction
!!        array coef_4_magnetic_ctl   1
!!          coef_4_magnetic_ctl       One                        1.0  end
!!        end array
!!        array coef_4_mag_p_ctl      1
!!          coef_4_mag_p_ctl          One                        1.0  end
!!        end array
!!        array coef_4_m_diffuse_ctl  1
!!          coef_4_m_diffuse_ctl      magnetic_Prandtl_number   -1.0  end
!!        end array
!!        array coef_4_induction_ctl  1
!!          coef_4_induction_ctl      One                       -1.0  end
!!        end array
!!      end  induction
!!   --------------------------------------------------------------------
!
      module t_ctl_data_induct_norm
!
      use m_precision
      use t_read_control_arrays
!
      implicit  none
!
!
!>      Structure for coefficients of magnetic induction equation
      type induction_equation_control
!>        Structure for number and power to construct
!!               evolution of magnetic field term
!!@n        coef_4_magne_evo%c_tbl:  Name of number 
!!@n        coef_4_magne_evo%vect:   Power of the number
        type(ctl_array_cr) :: coef_4_magne_evo
!
!>        Structure for number and power to construct
!!               magnetic diffusion term
!!@n        coef_4_mag_diffuse%c_tbl:  Name of number 
!!@n        coef_4_mag_diffuse%vect:   Power of the number
        type(ctl_array_cr) :: coef_4_mag_diffuse
!
!>        Structure for number and power to construct
!!               gradient of potenrial
!!@n        coef_4_mag_potential%c_tbl:  Name of number 
!!@n        coef_4_mag_potential%vect:   Power of the number
        type(ctl_array_cr) :: coef_4_mag_potential
!
!>        Structure for number and power to construct magnetic induction
!!@n        coef_4_induction%c_tbl:  Name of number 
!!@n        coef_4_induction%vect:   Power of the number
        type(ctl_array_cr) :: coef_4_induction
      end type induction_equation_control
!
!   5th level for coefs for induction
!
      character(len=kchara) :: hd_n_magne =  'coef_4_magnetic_ctl'
      character(len=kchara) :: hd_n_mag_p =  'coef_4_mag_p_ctl'
      character(len=kchara) :: hd_n_m_diff = 'coef_4_m_diffuse_ctl'
      character(len=kchara) :: hd_n_induct = 'coef_4_induction_ctl'
!
      private :: hd_n_magne, hd_n_mag_p
      private :: hd_n_m_diff, hd_n_induct
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_induction_ctl(hd_block, iflag, induct_ctl)
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      character(len=kchara), intent(in) :: hd_block
!
      integer(kind = kint), intent(inout) :: iflag
      type(induction_equation_control), intent(inout) :: induct_ctl
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if (iflag .gt. 0) return
      do
        call load_ctl_label_and_line
!
        iflag = find_control_end_flag(hd_block)
        if(iflag .gt. 0) exit
!
!
        call read_control_array_c_r                                     &
     &     (hd_n_magne, induct_ctl%coef_4_magne_evo)
        call read_control_array_c_r                                     &
     &     (hd_n_mag_p, induct_ctl%coef_4_mag_potential)
        call read_control_array_c_r                                     &
     &     (hd_n_m_diff, induct_ctl%coef_4_mag_diffuse)
        call read_control_array_c_r                                     &
     &     (hd_n_induct, induct_ctl%coef_4_induction)
      end do
!
      end subroutine read_induction_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_induction_ctl(induct_ctl)
!
      use bcast_control_arrays
!
      type(induction_equation_control), intent(inout) :: induct_ctl
!
!
      call bcast_ctl_array_cr(induct_ctl%coef_4_magne_evo)
      call bcast_ctl_array_cr(induct_ctl%coef_4_mag_potential)
      call bcast_ctl_array_cr(induct_ctl%coef_4_mag_diffuse)
      call bcast_ctl_array_cr(induct_ctl%coef_4_induction)
!
      end subroutine bcast_induction_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_induction_ctl(induct_ctl)
!
      type(induction_equation_control), intent(inout) :: induct_ctl
!
!
      call dealloc_control_array_c_r(induct_ctl%coef_4_magne_evo)
      call dealloc_control_array_c_r(induct_ctl%coef_4_mag_potential)
      call dealloc_control_array_c_r(induct_ctl%coef_4_mag_diffuse)
      call dealloc_control_array_c_r(induct_ctl%coef_4_induction)
!
      end subroutine dealloc_induction_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_induct_norm
