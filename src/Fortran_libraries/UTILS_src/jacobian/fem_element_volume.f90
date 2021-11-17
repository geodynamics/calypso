!
!     module fem_element_volume
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on Aug., 2006
!        Modified by H. Matsui on June, 2007
!
!!       subroutine fem_element_volume_pg(ele, g_FEM, jac_3d, n_int)
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(element_data), intent(inout) :: ele
!
      module fem_element_volume
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
      private :: s_fem_element_volume
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
       subroutine fem_element_volume_pg(ele, g_FEM, jac_3d, n_int)
!
      use t_fem_gauss_int_coefs
      use t_geometry_data
      use t_jacobians
!
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: n_int
      type(element_data), intent(inout) :: ele
!
!
      call s_fem_element_volume(ele%numele, ele%istack_ele_smp,         &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, n_int, jac_3d%ntot_int, jac_3d%xjac,             &
     &    ele%volume_ele, ele%a_vol_ele)
!
      end subroutine fem_element_volume_pg
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine s_fem_element_volume(numele, iele_smp_stack,          &
     &           max_int_point, maxtot_int_3d, int_start3, owe3d,       &
     &           n_int, ntot_int_3d, xjac, volume_ele, a_vol_ele)
!
      integer(kind=kint), intent(in) :: numele
      integer(kind=kint), intent(in) :: iele_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer(kind=kint), intent(in) :: ntot_int_3d, n_int
      real(kind=kreal),   intent(in) :: xjac(numele, ntot_int_3d)
!
      real (kind=kreal), intent(inout) :: volume_ele(numele)
      real (kind=kreal), intent(inout) :: a_vol_ele(numele)
!
      integer (kind=kint) :: ip, iele, ii, ix
      integer (kind=kint) :: istart, iend
!
!
!$omp workshare
      volume_ele(1:numele) = 0.0d0
!$omp end workshare
!
!$omp parallel do private(iele,ii,ix,istart,iend) 
       do ip = 1, np_smp
         istart = iele_smp_stack(ip-1)+1
         iend =   iele_smp_stack(ip)
!
         do ii=1, n_int * n_int * n_int
           ix = int_start3(n_int) + ii
           do iele = istart, iend
             volume_ele(iele) = volume_ele(iele)                        &
     &                         + xjac(iele,ix)*owe3d(ix)
           end do
         end do
!
!
         do iele = istart, iend
           if (volume_ele(iele).eq.0.0d0) then
             a_vol_ele(iele) = 1.0d60
           else
             a_vol_ele(iele) = 1.0d0 / volume_ele(iele)
           end if
         end do
       end do
!$omp end parallel do
!
       end subroutine s_fem_element_volume
!
!-----------------------------------------------------------------------
!
      end module fem_element_volume
