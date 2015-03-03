!>@file   m_radial_mat_sph_w_center.f90
!!@brief  module m_radial_mat_sph_w_center
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2013
!
!>@brief Radial band matrix for time evolutions
!!
!!@verbatim
!!      subroutine allocate_temp00_mat_sph
!!      subroutine allocate_press00_mat_sph
!!      subroutine allocate_comp00_mat_sph
!!
!!      subroutine deallocate_temp00_mat_sph
!!      subroutine deallocate_press00_mat_sph
!!      subroutine deallocate_comp00_mat_sph
!!
!!      subroutine check_press00_mat_sph(my_rank)
!!      subroutine check_temp00_mat_sph(my_rank)
!!      subroutine check_comp00_mat_sph(my_rank)
!!@endverbatim
!
      module m_radial_mat_sph_w_center
!
      use m_precision
!
      implicit none
!
!
!>     Temporal space for average pressure
      real(kind = kreal), allocatable :: p00_solution(:)
!>     Band matrix for Poisson equation of average pressure
      real(kind = kreal), allocatable :: p00_poisson_mat(:,:)
!>     LU decompositted matrix for Poisson equation of average  pressure
      real(kind = kreal), allocatable :: p00_poisson_lu(:,:)
!>     Source of detarminant for Poisson equation of average pressure
      real(kind = kreal), allocatable :: p00_poisson_det(:)
!>     Pivot table for Poisson equation of average pressure
      integer(kind = kint), allocatable :: i_p00_pivot(:)
!
!
!>     Temporal space for average temperature
      real(kind = kreal), allocatable :: t00_solution(:)
!>     Band matrix for time evolution of average temperature
      real(kind = kreal), allocatable :: t00_evo_mat(:,:)
!>     LU decompositted matrix for time evolution
!!     of average temperature
      real(kind = kreal), allocatable :: t00_evo_lu(:,:)
!>     Source of detarminant for time evolution
!!     of average temperature
      real(kind = kreal), allocatable :: t00_evo_det(:)
!>     Pivot table for time evolution of average temperature
      integer(kind = kint), allocatable :: i_t00_pivot(:)
!
!
!>     Temporal space for average composition
      real(kind = kreal), allocatable :: c00_solution(:)
!>     Band matrix for time evolution of average composition
      real(kind = kreal), allocatable :: c00_evo_mat(:,:)
!>     LU decompositted matrix for time evolution
!!     of average composition
      real(kind = kreal), allocatable :: c00_evo_lu(:,:)
!>     Source of detarminant for time evolution
!!     of average composition
      real(kind = kreal), allocatable :: c00_evo_det(:)
!>     Pivot table for time evolution of average composition
      integer(kind = kint), allocatable :: i_c00_pivot(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_temp00_mat_sph
!
      use m_spheric_parameter
!
      integer(kind = kint) :: nri
!
!
      nri =  nidx_rj(1)
!
      allocate( t00_solution(0:nri) )
      allocate( t00_evo_mat(3,0:nri) )
      allocate( t00_evo_lu(5,0:nri) )
      allocate( t00_evo_det(0:nri) )
      allocate( i_t00_pivot(0:nri) )
!
      t00_solution = 0.0d0
      t00_evo_mat =  0.0d0
      t00_evo_lu =   0.0d0
      t00_evo_det =  0.0d0
      i_t00_pivot =   0
      t00_evo_mat(2,0:nri) = 1.0d0
!
      end subroutine allocate_temp00_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine allocate_press00_mat_sph
!
      use m_spheric_parameter
!
      integer(kind = kint) :: nri
!
!
      nri = nidx_rj(1)
!
      allocate( p00_solution(0:nri) )
      allocate( p00_poisson_mat(3,0:nri) )
      allocate( p00_poisson_lu(5,0:nri) )
      allocate( p00_poisson_det(0:nri) )
      allocate( i_p00_pivot(0:nri) )
!
      p00_solution =    0.0d0
      p00_poisson_mat = 0.0d0
      p00_poisson_lu =  0.0d0
      p00_poisson_det = 0.0d0
      i_p00_pivot =     0
!
      end subroutine allocate_press00_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine allocate_comp00_mat_sph
!
      use m_spheric_parameter
!
      integer(kind = kint) :: nri
!
!
      nri =  nidx_rj(1)
!
      allocate( c00_solution(0:nri) )
      allocate( c00_evo_mat(3,0:nri) )
      allocate( c00_evo_lu(5,0:nri) )
      allocate( c00_evo_det(0:nri) )
      allocate( i_c00_pivot(0:nri) )
!
      c00_solution = 0.0d0
      c00_evo_mat =  0.0d0
      c00_evo_lu =   0.0d0
      c00_evo_det =  0.0d0
      i_c00_pivot =      0
      c00_evo_mat(2,0:nri) = 1.0d0
!
      end subroutine allocate_comp00_mat_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_temp00_mat_sph
!
!
      deallocate( t00_solution )
      deallocate( t00_evo_mat, t00_evo_lu )
      deallocate( t00_evo_det, i_t00_pivot)
!
      end subroutine deallocate_temp00_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_press00_mat_sph
!
!
      deallocate( p00_solution )
      deallocate( p00_poisson_mat, p00_poisson_lu )
      deallocate( p00_poisson_det, i_p00_pivot )
!
      end subroutine deallocate_press00_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_comp00_mat_sph
!
!
      deallocate( c00_solution )
      deallocate( c00_evo_mat, c00_evo_lu )
      deallocate( c00_evo_det, i_c00_pivot )
!
      end subroutine deallocate_comp00_mat_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_press00_mat_sph(my_rank)
!
      use m_spheric_parameter
      use check_sph_radial_mat
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      write(50+my_rank,'(a)') 'poisson matrix for average  pressure'
      call check_radial_3band_mat_w_ctr(my_rank, nidx_rj(1),            &
     &    radius_1d_rj_r, p00_poisson_mat)
!
      end subroutine check_press00_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine check_temp00_mat_sph(my_rank)
!
      use m_spheric_parameter
      use check_sph_radial_mat
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      write(50+my_rank,'(a)') 'evolution matrix for ave. temperature'
      call check_radial_3band_mat_w_ctr(my_rank, nidx_rj(1),            &
     &    radius_1d_rj_r, t00_evo_mat)
!
      end subroutine check_temp00_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine check_comp00_mat_sph(my_rank)
!
      use m_spheric_parameter
      use check_sph_radial_mat
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      write(50+my_rank,'(a)')  'evolution matrix for ave. composition'
      call check_radial_3band_mat_w_ctr(my_rank, nidx_rj(1),            &
     &    radius_1d_rj_r, c00_evo_mat)
!
      end subroutine check_comp00_mat_sph
!
! -----------------------------------------------------------------------
!
      end module m_radial_mat_sph_w_center
