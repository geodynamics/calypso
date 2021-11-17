!>@file  sum_volume_of_domain.f90
!!       module sum_volume_of_domain
!!
!!@author H. Matsui and H.Okuda
!!@date    programmed by H.Matsui and H.Okuda
!!@n                                    in July 2000 (ver 1.1)
!!@n        Modified by H. Matsui in Aug., 2006
!!
!> @brief get local volume size for internal subdomain
!!
!!@verbatim
!!       subroutine allocate_volume_4_smp
!!       subroutine deallocate_volume_4_smp
!!
!!      subroutine sum_4_volume(numele, interior_ele, iele_fsmp_stack,  &
!!     &          volume_ele, vol_local)
!!       subroutine sum_of_volume_by_ele_table(numele, interior_ele,    &
!!     &           volume_ele, numele_field, iele_fsmp_stack,           &
!!     &           iele_field, vol_local)
!!@endverbatim
!
      module sum_volume_of_domain
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
      real(kind=kreal), allocatable :: xvol_smp(:)
      private :: xvol_smp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
       subroutine allocate_volume_4_smp
!
       allocate ( xvol_smp(np_smp) )
       xvol_smp = 0.0d0
!
       end subroutine allocate_volume_4_smp
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_volume_4_smp
!
       deallocate ( xvol_smp )
!
       end subroutine deallocate_volume_4_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sum_4_volume(numele, interior_ele, iele_fsmp_stack,    &
     &          volume_ele, vol_local)
!
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: interior_ele(numele)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: volume_ele(numele)
!
      real (kind=kreal), intent(inout) :: vol_local
!
      integer (kind=kint) :: iproc, iele
      integer (kind=kint) :: istart, iend
!
!
      vol_local = 0.0d0
      xvol_smp = 0.0d0
!
!$omp parallel do private(iele,istart,iend)
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend = iele_fsmp_stack(iproc)
        do iele = istart, iend
!
          xvol_smp(iproc) = xvol_smp(iproc)                             &
     &                     + volume_ele(iele)*dble(interior_ele(iele))
!
        end do
      end do
!$omp end parallel do
!
!cdir noconcur
      do iproc = 1, np_smp
        vol_local = vol_local + xvol_smp(iproc)
      end do
!
      end subroutine sum_4_volume
!
!-----------------------------------------------------------------------
!
       subroutine sum_of_volume_by_ele_table(numele, interior_ele,      &
     &           volume_ele, numele_field, iele_fsmp_stack,             &
     &           iele_field, vol_local)
!
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: interior_ele(numele)
      real (kind=kreal), intent(in) :: volume_ele(numele)
!
      integer (kind=kint), intent(in) :: numele_field
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: iele_field(numele_field)
!
      real (kind=kreal), intent(inout) :: vol_local
!
      integer (kind=kint) :: iproc, inum, iele
      integer (kind=kint) :: istart, iend
!
!
      vol_local = 0.0d0
      xvol_smp = 0.0d0
!
!$omp parallel do private(inum,iele,istart,iend)
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend = iele_fsmp_stack(iproc)
        do inum = istart, iend
!
          iele = iele_field(inum)
          xvol_smp(iproc) = xvol_smp(iproc)                             &
     &                      + volume_ele(iele)*dble(interior_ele(iele))
!
        end do
       end do
!$omp end parallel do
!
!poption noparallel
!cdir noconcur
      do iproc = 1, np_smp
        vol_local = vol_local + xvol_smp(iproc)
      end do
!
      end subroutine sum_of_volume_by_ele_table
!
!-----------------------------------------------------------------------
!
      end module sum_volume_of_domain
