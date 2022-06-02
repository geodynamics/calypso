!>@file   t_CMB_dipolarity.f90
!!@brief      module t_CMB_dipolarity
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2022
!
!> @brief  Evaluate dipolarity at CMB
!!
!!@verbatim
!!      subroutine set_ctl_dipolarity_params                            &
!!     &         (fdip_file_prefix, fdip_truncation, rj_fld, dip)
!!        type(read_character_item), intent(in) :: fdip_file_prefix
!!        type(read_integer_item), intent(in) :: fdip_truncation
!!        type(phys_data), intent(in) :: rj_fld
!!        type(dipolarity_data), intent(inout) :: dip
!!      subroutine write_dipolarity(id_rank, i_step, time, radius_CMB,  &
!!     &                            ipol, pwr, dip)
!!        integer, intent(in) :: id_rank
!!        integer(kind = kint), intent(in) :: i_step
!!        real(kind = kreal), intent(in) :: time
!!        real(kind = kreal), intent(in) :: radius_CMB
!!        type(phys_address), intent(in) :: ipol
!!        type(sph_mean_squares), intent(in) :: pwr
!!        type(dipolarity_data), intent(in) :: dip
!!      subroutine cal_CMB_dipolarity(id_rank, rj_fld, pwr, dip)
!!        integer, intent(in) :: id_rank
!!        type(phys_data), intent(in) :: rj_fld
!!        type(sph_mean_squares), intent(in) :: pwr
!!        real(kind = kreal), intent(inout) :: f_dip
!!@endverbatim
!
      module t_CMB_dipolarity
!
      use m_precision
      use m_constants
!
      use t_phys_data
      use t_rms_4_sph_spectr
!
      implicit none
!
!>        Field label for dipolarity
!!         @f$ f_{dip} @f$
      type(field_def), parameter :: dipolarity                          &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'dipolarity',                              &
     &                math = '$ f_{dip} $')
!
!
      type dipolarity_data
!>        Integer flag for dipolarity
        integer(kind = kint) :: iflag_dipolarity = 0
!>        File prefix for dipolarity data
        character(len = kchara) :: dipolarity_prefix= 'dipolarity'
!
!>        Radial address for dipolarity
        integer(kind = kint) :: krms_CMB
!>        magnetic energy address
        integer(kind = kint) :: icomp_mene = 0
!>        Truncation degree to evaluate dipolarity
        integer(kind = kint) :: ltr_max
!
!>        Dipolarity
        real(kind = kreal) :: f_dip
      end type dipolarity_data
!
      integer(kind = kint), parameter, private :: id_dipolarity = 36
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_dipolarity_params                              &
     &         (fdip_file_prefix, fdip_truncation, rj_fld, dip)
!
      use m_base_field_labels
      use t_phys_data
      use t_control_array_character
      use t_control_array_integer
!
      type(read_character_item), intent(in) :: fdip_file_prefix
      type(read_integer_item), intent(in) :: fdip_truncation
      type(phys_data), intent(in) :: rj_fld
      type(dipolarity_data), intent(inout) :: dip
!
      integer(kind = kint) :: i
!
!    Turn On Nusselt number if temperature gradient is there
      dip%iflag_dipolarity = 0
      do i = 1, rj_fld%num_phys
        if(rj_fld%phys_name(i) .eq. magnetic_field%name) then
          dip%iflag_dipolarity = 1
          exit
        end if
      end do
!
      if(fdip_file_prefix%iflag .gt. 0) then
        dip%iflag_dipolarity = 1
        dip%dipolarity_prefix = fdip_file_prefix%charavalue
      else
        dip%iflag_dipolarity = 0
      end if
!
      dip%ltr_max = -1
      if(fdip_truncation%iflag .gt. 0) then
        dip%ltr_max = fdip_truncation%intvalue
      end if
!
      end subroutine set_ctl_dipolarity_params
!
! -----------------------------------------------------------------------
!
      subroutine write_dipolarity(id_rank, i_step, time, radius_CMB,    &
     &                            ipol, pwr, dip)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      real(kind = kreal), intent(in) :: radius_CMB
      type(phys_address), intent(in) :: ipol
      type(sph_mean_squares), intent(in) :: pwr
      type(dipolarity_data), intent(in) :: dip
!
!
      if(dip%iflag_dipolarity .le. izero) return
      if(ipol%base%i_magne .le. 0) return
      if(id_rank .ne. pwr%irank_l) return
!
      call open_dipolarity_file(dip, radius_CMB)
!
      write(id_dipolarity,'(i16,1pe23.14e3)',advance='NO') i_step, time
      write(id_dipolarity,'(1pe23.14e3)') dip%f_dip
      close(id_dipolarity)
!
      end subroutine write_dipolarity
!
! -----------------------------------------------------------------------
!
      subroutine cal_CMB_dipolarity(id_rank, rj_fld, pwr, dip)
!
      use t_spheric_parameter
      use t_spheric_rj_data
!
      integer, intent(in) :: id_rank
      type(phys_data), intent(in) :: rj_fld
      type(sph_mean_squares), intent(in) :: pwr
!
      type(dipolarity_data), intent(inout) :: dip
!
!>        magnetic energy at CMB
        real(kind = kreal) :: me_cmb_d(3)
!>        dipole component of magnetic energy at CMB
        real(kind = kreal) :: pwr_g10
!
      integer(kind = kint) :: l
!
!
      if(dip%iflag_dipolarity .le. izero) return
!
      if(dip%icomp_mene .le. 0)                                         &
     &          dip%icomp_mene = find_rms_address_4_mene(pwr, rj_fld)
      if(dip%icomp_mene .le. 0) return
!
      if(id_rank .eq. pwr%irank_l) then
        pwr_g10 = pwr%shl_l(dip%krms_CMB,1,dip%icomp_mene)
!
        me_cmb_d(1:3) = 0.0d0
        do l = 1, dip%ltr_max
          me_cmb_d(1) = me_cmb_d(1)                                     &
     &                 + pwr%shl_l(dip%krms_CMB,l,dip%icomp_mene)
          me_cmb_d(2) = me_cmb_d(2)                                     &
     &                 + pwr%shl_l(dip%krms_CMB,l,dip%icomp_mene)
          me_cmb_d(3) = me_cmb_d(3)                                     &
     &                 + pwr%shl_l(dip%krms_CMB,l,dip%icomp_mene)
        end do
        dip%f_dip = pwr_g10 / me_cmb_d(1)
      end if
!
      end subroutine cal_CMB_dipolarity
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_dipolarity_file(dip, radius_CMB)
!
      use set_parallel_file_name
      use write_field_labels
!
      type(dipolarity_data), intent(in) :: dip
      real(kind = kreal), intent(in) :: radius_CMB
      character(len = kchara) :: file_name
!
!
      file_name = add_dat_extension(dip%dipolarity_prefix)
      open(id_dipolarity, file = file_name,                             &
     &    form='formatted', status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_dipolarity, file = file_name,                             &
     &    form='formatted', status='replace')
!
!
      write(id_dipolarity,'(a)')    '# Truncation   CMB_radius'
      write(id_dipolarity,'(i16,1pe25.15e3)')                           &
     &                         dip%ltr_max, radius_CMB
!
      write(id_dipolarity,'(a)',advance='NO')                           &
     &    't_step    time    f_dip'
      write(id_dipolarity,'(a)') ''
!
      end subroutine open_dipolarity_file
!
! -----------------------------------------------------------------------
!
      integer(kind = kint)                                              &
     &          function find_rms_address_4_mene(pwr, rj_fld)
!
      use m_base_field_labels
      use t_rms_4_sph_spectr
!
      type(phys_data), intent(in) :: rj_fld
      type(sph_mean_squares), intent(in) :: pwr
!
      integer(kind = kint) :: j_fld, i_fld
!
      do j_fld = 1, pwr%num_fld_sq
        i_fld = pwr%id_field(j_fld)
!
        find_rms_address_4_mene = 0
        if(rj_fld%phys_name(i_fld) .eq. magnetic_field%name) then
          find_rms_address_4_mene = pwr%istack_comp_sq(j_fld-1) + 1
          exit
        end if
      end do
!
      end function find_rms_address_4_mene
!
! -----------------------------------------------------------------------
!
      end module t_CMB_dipolarity
