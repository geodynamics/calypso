!>@file   m_property_flags.f90
!!@brief  module m_property_flags
!!
!!@author H. Matsui
!!@date Programmed in 2001
!!@date Modified in Jan., 2007
!
!>@brief  Coeffiecients for a scalar field
!!
!!@verbatim
!!      subroutine set_filtered_advection_ctl(filterd_advect_ctl,       &
!!     &                                      scl_prop)
!!        type(read_character_item), intent(in) :: filterd_advect_ctl
!!        type(scalar_property), intent(inout) :: scl_prop
!!@endverbatim
!
      module m_property_flags
!
      use m_precision
      use m_constants
!
      implicit  none
!
!>      Scheme ID for no evolution
      integer (kind=kint), parameter :: id_no_evolution =         0
!>      Scheme ID for explicit Euler scheme
      integer (kind=kint), parameter :: id_explicit_euler =       1
!>      Scheme ID for 2nd order Adams-Bashforth Scheme
      integer (kind=kint), parameter :: id_explicit_adams2 =      2
!>      Scheme ID for Crank-Nicolson Scheme
      integer (kind=kint), parameter :: id_Crank_nicolson =       3
!>      Scheme ID for Crank-Nicolson Scheme with consistent mass matrix
      integer (kind=kint), parameter :: id_Crank_nicolson_cmass = 4
!
      end module m_property_flags
