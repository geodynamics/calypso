!>@file   m_phys_constants.f90
!!@brief  module m_phys_constants
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Matsui on MAy., 2009
!
!> @brief Constants for physical data
!!
      module m_phys_constants
!
      use m_precision
!
      implicit  none
!
!
!>      Number of components for scalar
      integer (kind = kint), parameter :: n_scalar = 1
!>      Number of components for vector
      integer (kind = kint), parameter :: n_vector = 3
!>      Number of components for symmetric tensor
      integer (kind = kint), parameter :: n_sym_tensor =  6
!>      Number of components for asymmetric tensor
      integer (kind = kint), parameter :: n_asym_tensor = 3
!>      Number of components for solenoidal field
      integer (kind = kint), parameter :: n_solenoid = 2
!
!>      Component table for symmetric tensor
      integer (kind = kint), parameter :: l_sim_t(3,3)                  &
     &                       = reshape( (/0, 1, 2,                      &
     &                                    1, 3, 4,                      &
     &                                    2, 4, 5/), shape=(/3,3/))
!
!>      Component table for asymmetric tensor
      integer (kind = kint), parameter :: l_asim_t(3,3,2)               &
     &                = reshape( (/0, 0, 2,   0, 0, 1,  2, 1, 0,        &
     &                             0, 1,-1,  -1, 0, 1,  1,-1, 0 /),     &
     &                  shape=(/3,3,2/) )
!
!>      Address for diagonal component for symmetric tensor
      integer (kind = kint), parameter :: lst_sim_t(3) = (/0, 3, 5/)
!
      end module m_phys_constants
