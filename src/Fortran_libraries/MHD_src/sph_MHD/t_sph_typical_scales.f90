!>@file   t_sph_typical_scales.f90
!!@brief      module t_sph_typical_scales
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2022
!
!> @brief  Evaluate dipolarity at CMB
!!
!!@verbatim
!!      subroutine set_ctl_typical_scale_params                         &
!!     &         (typ_scale_file_prefix, rj_fld, tsl)
!!        type(read_character_item), intent(in) :: typ_scale_file_prefix
!!        type(phys_data), intent(in) :: rj_fld
!!        type(typical_scale_data), intent(inout) :: tsl
!!      subroutine write_typical_scales(i_step, time, pwr, tsl)
!!        integer(kind = kint), intent(in) :: i_step
!!        real(kind = kreal), intent(in) :: time
!!        type(sph_mean_squares), intent(in) :: pwr
!!        type(typical_scale_data), intent(in) :: tsl
!!      subroutine cal_typical_scales(rj_fld, pwr, tsl)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        real(kind = kreal), intent(in)                                &
!!     &      :: g_sph_rj(sph_rj%nidx_rj(2),13)
!!        type(sph_mean_square_work), intent(inout) :: WK_pwr
!!        real(kind = kreal), intent(inout) :: f_dip
!!@endverbatim
!
      module t_sph_typical_scales
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_phys_data
!
      implicit none
!
!>        Field label for Elsasser number
!!         @f$ B^{2} / \rho \mu_{0} \eta \Omega @f$
      type(field_def), parameter :: Elsasser_number                     &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Elsasser_number',                         &
     &                math = '$B^{2} / \rho \mu_{0} \eta \Omega $')
!
!>        Field label for dynamic Elsasser number
!!         @f$ B^{2}/ \rho \mu_{0} \Omega u \ell_{B} @f$
      type(field_def), parameter :: dynamic_Elsasser_number             &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'dynamic_Elsasser_number',                 &
     &                math = '$B^{2}/ \rho \mu_{0} \Omega u \ell_{B}$')
!
!>        Field label for dynamic Alfven number
!!         @f$ u \sqrt{\rho \mu_{0}} / B @f$
      type(field_def), parameter :: Alfven_number                       &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Alfven_number',                           &
     &                math = '$u \sqrt{\rho \mu_{0}} / B$')
!
!>        Field label for dynamic Lehnert number
!!         @f$ B / \ell_{B} \Omega \sqrt{\rho \mu_{0}} @f$
      type(field_def), parameter :: Lehnert_number                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Lehnert_number',                          &
     &              math = '$B / \ell_{B} \Omega \sqrt{\rho \mu_{0}}$')
!
!>        Field label for typical flow degree
!!         @f$ \bar{l_{u}} @f$
      type(field_def), parameter :: flow_degree                         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'flow_degree',                             &
     &                math = '$ \bar{l_{u}} $')
!
!>        Field label for typical flow order
!!         @f$ \bar{m_{u}} @f$
      type(field_def), parameter :: flow_order                          &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'flow_order',                              &
     &                math = '$ \bar{m_{u}} $')
!
!>        Field label for typical flow degree
!!         @f$ \bar{l_{B}} @f$
      type(field_def), parameter :: magne_degree                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'magne_degree',                            &
     &                math = '$ \bar{l_{u}} $')
!
!>        Field label for typical flow order
!!         @f$ \bar{m_{B}} @f$
      type(field_def), parameter :: magne_order                         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'magne_order',                             &
     &                math = '$ \bar{m_{u}} $')
!
!
      type typical_scale_data
!>        Integer flag for typical scales
        integer(kind = kint) :: iflag_ub_scales = 0
!>        File prefix for dipolarity data
        character(len = kchara) :: scale_prefix = 'typical_scales'
!
!>        kinetic energy address
        integer(kind = kint) :: icomp_kene = 0
!>        magnetic energy address
        integer(kind = kint) :: icomp_mene = 0
!
!>        magnetic length scale
        real(kind = kreal) :: dl_mag
!>        magnetic zonal length scale
        real(kind = kreal) :: dm_mag
!>        kinetic length scale
        real(kind = kreal) :: dl_kin
!>        kinetic zonal length scale
        real(kind = kreal) :: dm_kin
      end type typical_scale_data
!
      integer(kind = kint), parameter, private :: id_scale = 36
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_typical_scale_params                           &
     &         (typ_scale_file_prefix, rj_fld, tsl)
!
      use m_base_field_labels
      use t_phys_data
      use t_control_array_character
!
      type(read_character_item), intent(in) :: typ_scale_file_prefix
      type(phys_data), intent(in) :: rj_fld
      type(typical_scale_data), intent(inout) :: tsl
!
      integer(kind = kint) :: i
!
!    Turn On Nusselt number if temperature gradient is there
      tsl%iflag_ub_scales = 0
      do i = 1, rj_fld%num_phys
        if(rj_fld%phys_name(i) .eq. magnetic_field%name) then
          tsl%iflag_ub_scales = 1
          exit
        end if
        if(rj_fld%phys_name(i) .eq. velocity%name) then
          tsl%iflag_ub_scales = 1
          exit
        end if
      end do
!
      if(typ_scale_file_prefix%iflag .gt. 0) then
        tsl%iflag_ub_scales = 1
        tsl%scale_prefix = typ_scale_file_prefix%charavalue
      else
        tsl%iflag_ub_scales = 0
      end if
!
      end subroutine set_ctl_typical_scale_params
!
! -----------------------------------------------------------------------
!
      subroutine write_typical_scales(i_step, time, pwr, tsl)
!
      use t_rms_4_sph_spectr
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      type(sph_mean_squares), intent(in) :: pwr
      type(typical_scale_data), intent(in) :: tsl
!
!
      if(tsl%iflag_ub_scales .le. izero) return
      if((tsl%icomp_kene + tsl%icomp_mene) .le. 0) return
      if(my_rank .ne. pwr%v_spectr(1)%irank_m) return
!
      call open_dipolarity_file(tsl)
!
      write(id_scale,'(i16,1pe23.14e3)',advance='NO') i_step, time
      if(tsl%icomp_kene .gt. 0) then
         write(id_scale,'(1p2e23.14e3)',advance='NO')                   &
     &                                       tsl%dl_kin, tsl%dm_kin
      end if
      if(tsl%icomp_mene .gt. 0) then
         write(id_scale,'(1p2e23.14e3)',advance='NO')                   &
     &                                       tsl%dl_mag, tsl%dm_mag
      end if
      write(id_scale,'(a)') ''
      close(id_scale)
!
      end subroutine write_typical_scales
!
! -----------------------------------------------------------------------
!
      subroutine cal_typical_scales(rj_fld, pwr, tsl)
!
      use t_rms_4_sph_spectr
!
      type(phys_data), intent(in) :: rj_fld
      type(sph_mean_squares), intent(in) :: pwr
!
      type(typical_scale_data), intent(inout) :: tsl
!
!
      if(tsl%icomp_kene .le. 0)                                         &
     &          tsl%icomp_kene = find_rms_address_4_kene(pwr, rj_fld)
      if(tsl%icomp_mene .le. 0)                                         &
     &          tsl%icomp_mene = find_rms_address_4_mene(pwr, rj_fld)
!
      if((tsl%icomp_kene + tsl%icomp_mene) .le. 0) return
      if(tsl%iflag_ub_scales .le. izero) return
!
!
      if(tsl%icomp_kene .gt. 0) then
        call cal_typical_scale(tsl%icomp_kene, pwr%v_spectr(1),         &
     &                         tsl%dl_kin, tsl%dm_kin)
      end if
      if(tsl%icomp_mene .gt. 0) then
        call cal_typical_scale(tsl%icomp_mene, pwr%v_spectr(1),         &
     &                         tsl%dl_mag, tsl%dm_mag)
      end if
!
      end subroutine cal_typical_scales
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_dipolarity_file(tsl)
!
      use set_parallel_file_name
      use write_field_labels
!
      type(typical_scale_data), intent(in) :: tsl
      character(len = kchara) :: file_name
!
!
      file_name = add_dat_extension(tsl%scale_prefix)
      open(id_scale, file = file_name,                                  &
     &    form='formatted', status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_scale, file = file_name,                                  &
     &    form='formatted', status='replace')
!
!
      write(id_scale,'(a)',advance='NO')                                &
     &    't_step    time    '
      if(tsl%icomp_mene .gt. 0) write(id_scale,'(a)',advance='NO')      &
     &    'flow_degree  flow_order  '
      if(tsl%icomp_mene .gt. 0) write(id_scale,'(a)',advance='NO')      &
     &    'magnetc_degree  magnetic_order'
      write(id_scale,'(a)') ''
!
      end subroutine open_dipolarity_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint)                                              &
     &          function find_rms_address_4_kene(pwr, rj_fld)
!
      use m_base_field_labels
      use t_rms_4_sph_spectr
!
      type(phys_data), intent(in) :: rj_fld
      type(sph_mean_squares), intent(in) :: pwr
!
      integer(kind = kint) :: j_fld, i_fld
!
      find_rms_address_4_kene = 0
      do j_fld = 1, pwr%num_fld_sq
        i_fld = pwr%id_field(j_fld)
!
        if(rj_fld%phys_name(i_fld) .eq. velocity%name) then
          find_rms_address_4_kene =  pwr%istack_comp_sq(j_fld-1) + 1
          exit
        end if
      end do
      end function find_rms_address_4_kene
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
      subroutine cal_typical_scale(icomp_mene, v_pwr, dl_mag, dm_mag)
!
      use t_sph_volume_mean_square
      use calypso_mpi_real
!
      integer(kind = kint), intent(in) :: icomp_mene
      type(sph_vol_mean_squares), intent(in) :: v_pwr
      real(kind = kreal), intent(inout) :: dl_mag, dm_mag
!
      integer(kind = kint) :: l, m
!
!
      dl_mag = 0.0d0
      if(my_rank .eq. v_pwr%irank_l) then
        do l = 1, v_pwr%ltr
          dl_mag = dl_mag + dble(l) * v_pwr%v_l(l,icomp_mene+2)
        end do
      end if
      call calypso_mpi_bcast_one_real(dl_mag, v_pwr%irank_l)
!
      dm_mag = 0.0d0
      if(my_rank .eq. v_pwr%irank_m) then
        do m = 1, v_pwr%ltr
          dm_mag = dm_mag + dble(m) * v_pwr%v_m(m,icomp_mene+2)
        end do
        dm_mag = dm_mag / v_pwr%v_sq(icomp_mene+2)
        dl_mag = dl_mag / v_pwr%v_sq(icomp_mene+2)
      end if
!
      end subroutine cal_typical_scale
!
! -----------------------------------------------------------------------
!
      end module t_sph_typical_scales
