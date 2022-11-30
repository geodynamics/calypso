!>@file   t_sph_typical_scales.f90
!!@brief      module t_sph_typical_scales
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2022
!
!> @brief  Evaluate dipolarity at CMB
!!
!!@verbatim
!!      subroutine alloc_typical_scale_data(num, tsl)
!!      subroutine dealloc_typical_scale_data(tsl)
!!        integer(kind = kint), intent(in) :: num
!!        type(typical_scale_data), intent(inout) :: tsl
!!@endverbatim
!
      module t_sph_typical_scales
!
      use m_precision
      use m_constants
!
      use t_field_labels
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
!>        gzipped flag for dipolarity data
        logical :: flag_gzip_scale = .FALSE.
!
!>        kinetic energy address
        integer(kind = kint) :: icomp_kene = 0
!>        magnetic energy address
        integer(kind = kint) :: icomp_mene = 0
!
!>        number of output component
        integer(kind = kint) :: num_lscale = 0
!>        number of output component
        integer(kind = kint), allocatable :: ncomp_lscale(:)
!>        number of output component
        character(len = kchara), allocatable :: lscale_name(:)
!>        magnetic length scale
        real(kind = kreal) :: dl_mag
!>        magnetic zonal length scale
        real(kind = kreal) :: dm_mag
!>        magnetic meridional length scale
        real(kind = kreal) :: dlm_mag
!
!>        kinetic length scale
        real(kind = kreal) :: dl_kin
!>        kinetic zonal length scale
        real(kind = kreal) :: dm_kin
!>        magnetic meridional length scale
        real(kind = kreal) :: dlm_kin
      end type typical_scale_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_typical_scale_data(num, tsl)
!
      integer(kind = kint), intent(in) :: num
      type(typical_scale_data), intent(inout) :: tsl
!
      tsl%num_lscale = num
      allocate(tsl%lscale_name(tsl%num_lscale))
      allocate(tsl%ncomp_lscale(tsl%num_lscale))
!
      if(tsl%num_lscale .gt. 0) then
        tsl%ncomp_lscale(1:tsl%num_lscale) = 1
      end if
!
      end subroutine alloc_typical_scale_data
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_typical_scale_data(tsl)
!
      type(typical_scale_data), intent(inout) :: tsl
!
      if(allocated(tsl%ncomp_lscale) .eqv. .FALSE.) return
      deallocate(tsl%ncomp_lscale, tsl%lscale_name)
!
      end subroutine dealloc_typical_scale_data
!
! -----------------------------------------------------------------------
!
      end module t_sph_typical_scales
