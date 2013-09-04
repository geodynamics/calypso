!> @file  m_t_int_parameter.f90
!!      module m_t_int_parameter
!!
!! @author H. Matsui and H. Okuda
!! @date Written on Oct., 2000
!!
!> @brief Parameters for time integration
!
      module m_t_int_parameter
!
      use m_precision
      use m_constants
!
      implicit  none
!
!>      length of each time step @f$ \Delta t @f$
      real(kind=kreal) :: dt
!
!>      @f$ 1 / \Delta t @f$
      real(kind=kreal) :: ddt
!
!>      significand of @f$ \Delta t @f$
      real(kind=kreal) :: dt_fact
!>      exponent of @f$ \Delta t @f$
      integer(kind = kint) :: idt_digit
!
!>     Fixed time step flag
      integer(kind=kint), parameter :: iflag_fixed_step = 0
!>     Flexible time step flag
      integer(kind=kint), parameter :: iflag_flex_step =  1
!>     flag for time stepping
      integer(kind=kint) :: iflag_flexible_step = iflag_fixed_step
!
!>      Maximum @f$ \Delta t @f$
      real(kind=kreal) :: dt_max
!>      Mimimum @f$ \Delta t @f$
      real(kind=kreal) :: dt_min
!
!>      Maximum error to shrink time step
      real(kind=kreal) :: max_eps_to_shrink_dt
!>      Minimum error to expand time step
      real(kind=kreal) :: min_eps_to_expand_dt
!
!>      Coefficient of implicit viscous diffusion term
      real(kind=kreal) :: coef_imp_v
!>      Coefficient of implicit thermal diffusion term
      real(kind=kreal) :: coef_imp_t
!>      Coefficient of implicit magnetic diffusion term
      real(kind=kreal) :: coef_imp_b
!>      Coefficient of implicit compositional diffusion term
      real(kind=kreal) :: coef_imp_c
!
!>      Coefficient of explicit viscous diffusion term
      real(kind=kreal) :: coef_exp_v
!>      Coefficient of explicit thermal diffusion term
      real(kind=kreal) :: coef_exp_t
!>      Coefficient of explicit magnetic diffusion term
      real(kind=kreal) :: coef_exp_b
!>      Coefficient of explicit compositional diffusion term
      real(kind=kreal) :: coef_exp_c
!
!>      Coefficient of terms at current step for Adams-Bashforth
      real(kind=kreal), parameter :: adam_0 =  three / two
!>      Coefficient of terms at previous step for Adams-Bashforth
      real(kind=kreal), parameter :: adam_1 = -one / two
!>      1 / adam_0
      real(kind=kreal), parameter :: adam_r =  two / three
!
      end module m_t_int_parameter
