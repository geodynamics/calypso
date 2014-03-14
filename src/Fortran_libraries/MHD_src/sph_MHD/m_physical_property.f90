!>@file   m_physical_property.f90
!!@brief  module m_physical_property
!!
!!@author H. Matsui
!!@date Programmed in 2001
!!@date Modified in Jan., 2007
!
!>@brief  Coeffiecients of each term
!
      module m_physical_property
!
      use m_precision
!
      implicit  none
!
!
!>     flag for no gravity
      integer (kind = kint), parameter :: iflag_no_gravity =  -1
!>     flag for constant gravity
      integer (kind = kint), parameter :: iflag_const_g =      0
!>     flag for radial gravity (amplitude is fixed)
      integer (kind = kint), parameter :: iflag_radial_g =     1
!>     flag for self radial gravity
      integer (kind = kint), parameter :: iflag_self_r_g =     2
!
!>     rotation vector for Coriolis force
      real (kind=kreal) :: angular(3)
!>     external magnetic field (Constant)
      real (kind=kreal) :: ex_magne(3)
!
!>     flag for gravity type
      integer (kind=kint) :: i_grav
!>     gravity direction for constant gravity
      real (kind=kreal) :: grav(3)
!
!   Coefficients
!
!>     coefficient for time evolution of velocity and advection
      real  (kind=kreal) :: coef_velo
!>     coefficient for advection (-coef_velo)
      real  (kind=kreal) :: coef_nega_v
!>     coefficient for time pressure gradient
      real  (kind=kreal) :: coef_press
!>     coefficient for time evolution of temperature and heat flux
      real  (kind=kreal) :: coef_temp
!>     coefficient for heat flux (-coef_temp)
      real  (kind=kreal) :: coef_nega_t
!>     coefficient for time evolution of magnetic field
      real  (kind=kreal) :: coef_magne
!>     coefficient for time electric potentia
      real  (kind=kreal) :: coef_mag_p
!>     coefficient for time evolution of composition and composition flux
      real  (kind=kreal) :: coef_light
!>     coefficient for composition flux (-coef_light)
      real  (kind=kreal) :: coef_nega_c
!
!>     1 / coef_press
      real  (kind=kreal) :: acoef_press
!>     1 / coef_mag_p
      real  (kind=kreal) :: acoef_mag_p
!
!>     coefficient for viscous diffusion
      real  (kind=kreal) :: coef_d_velo
!>     coefficient for thermal diffusion
      real  (kind=kreal) :: coef_d_temp
!>     coefficient for magnetic diffusion
      real  (kind=kreal) :: coef_d_magne
!>     coefficient for chemical diffusion
      real  (kind=kreal) :: coef_d_light
!
!>     coefficient for thermal buoyancy
      real  (kind=kreal) :: coef_buo
!>     coefficient for Coriolis force
      real  (kind=kreal) :: coef_cor
!>     coefficient for chemical Lorentz force
      real  (kind=kreal) :: coef_lor
!>     coefficient for magnetic induction
      real  (kind=kreal) :: coef_induct
!>     coefficient for chemical buoyancy
      real  (kind=kreal) :: coef_comp_buo
!
!>     coefficient for heat source term
      real  (kind=kreal) :: coef_h_src
!>     coefficient for compositional source term
      real  (kind=kreal) :: coef_c_src
!
!>     Parameter for stratified layer (amplitude)
      real  (kind=kreal) :: stratified_sigma
!>     Parameter for stratified layer (thckness)
      real  (kind=kreal) :: stratified_width
!>     Parameter for stratified layer (radius)
      real  (kind=kreal) :: stratified_outer_r
!
      end module m_physical_property
