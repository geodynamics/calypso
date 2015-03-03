!reconnect_psf_overlap_nod.f90
!      module reconnect_psf_overlap_nod
!
!      Written by H. Matsui on Oct., 2011
!
!      subroutine s_reconnect_psf_overlap_nod(num_psf, ntot_nod_psf,    &
!     &          istack_nod_out, ihash_out_psf, xx_out_psf, psf_out)
!      subroutine set_global_psf_node_id(num_psf, psf_out)
!
      module reconnect_psf_overlap_nod
!
      use m_precision
      use calypso_mpi
!
      implicit  none
!
      integer(kind = kint), allocatable :: inod_overlap(:)
      integer(kind = kint), allocatable :: ihash(:)
      integer(kind = kint), allocatable :: inod_org(:)
      private :: inod_overlap, inod_org
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_reconnect_psf_overlap_nod(num_psf, ntot_nod_psf,     &
     &          istack_nod_out, ihash_out_psf, psf_out)
!
      use t_ucd_data
!
      use quicksort
!
      integer(kind = kint), intent(in) :: num_psf
      integer(kind = kint), intent(in) :: ntot_nod_psf
      integer(kind = kint), intent(in) :: istack_nod_out(0:num_psf)
      integer(kind = kint), intent(in) :: ihash_out_psf(ntot_nod_psf)
!
      type(ucd_data), intent(inout) :: psf_out(num_psf)
!
      integer(kind = kint) :: inod, jnod, i, j
      integer(kind = kint) :: i_psf, ist, ied, ishift
      integer(kind = kint) :: ii, jj
      integer(kind = kint_gl) :: iele, i1, i2, i3
!
!
      if (my_rank .ne. 0) return
!
      allocate(inod_overlap(ntot_nod_psf))
      allocate(inod_org(ntot_nod_psf))
      allocate(ihash(ntot_nod_psf))
!
!$omp parallel do
      do inod = 1, ntot_nod_psf
        inod_org(inod) = inod
        ihash(inod) = ihash_out_psf(inod)
      end do
!$omp end parallel do
!
      do i_psf = 1, num_psf
        ist = istack_nod_out(i_psf-1) + 1
        ied = istack_nod_out(i_psf)
        call quicksort_w_index(ntot_nod_psf, ihash, ist, ied, inod_org)
      end do
!
!
      inod_overlap(1:ntot_nod_psf) = 0
      do i_psf = 1, num_psf
        ist = istack_nod_out(i_psf-1) + 1
        ied = istack_nod_out(i_psf)
        do i = ist, ied
          inod = inod_org(i)
          ii= inod - istack_nod_out(i_psf-1)
!
          if(inod_overlap(inod) .eq. 0) then
            inod_overlap(inod) = inod
            do j = i+1, ied
              if(ihash(j) .ne. ihash(i)) exit
!
              jnod = inod_org(j)
              jj = inod_org(j) - istack_nod_out(i_psf-1)
              if(inod_overlap(jnod) .eq. 0) then
                if  (psf_out(i_psf)%xx(ii,1).eq.psf_out(i_psf)%xx(jj,1) &
     &         .and. psf_out(i_psf)%xx(ii,2).eq.psf_out(i_psf)%xx(jj,2) &
     &         .and. psf_out(i_psf)%xx(ii,3).eq.psf_out(i_psf)%xx(jj,3) &
     &           ) then
                  inod_overlap(jnod) = inod
                end if
              end if
            end do
          end if
!
        end do
      end do
!
!
      do i_psf = 1, num_psf
        ishift = istack_nod_out(i_psf-1)
!$omp parallel do private(iele,i1,i2,i3)
        do iele = 1, psf_out(i_psf)%nele
          i1 = psf_out(i_psf)%ie(iele,1) + ishift
          i2 = psf_out(i_psf)%ie(iele,2) + ishift
          i3 = psf_out(i_psf)%ie(iele,3) + ishift
!
          psf_out(i_psf)%iele_global(iele) = iele
          psf_out(i_psf)%ie(iele,1) = inod_overlap(i1) - ishift
          psf_out(i_psf)%ie(iele,2) = inod_overlap(i2) - ishift
          psf_out(i_psf)%ie(iele,3) = inod_overlap(i3) - ishift
        end do
!$omp end parallel do
      end do
!
      deallocate(inod_overlap, inod_org, ihash)
!
      end subroutine s_reconnect_psf_overlap_nod
!
! ----------------------------------------------------------------------
!
      subroutine set_global_psf_node_id(num_psf, psf_out)
!
      use t_ucd_data
!
      integer(kind = kint), intent(in) :: num_psf
      type(ucd_data), intent(inout) :: psf_out(num_psf)
!
      integer(kind = kint) :: i_psf
      integer(kind = kint_gl) :: inum
!
!
      if(my_rank .gt. 0) return
!
      do i_psf = 1, num_psf
        do inum = 1, psf_out(i_psf)%nnod
          psf_out(i_psf)%inod_global(inum) = inum
        end do
      end do
!
      end subroutine set_global_psf_node_id
!
! ----------------------------------------------------------------------
!
      end module reconnect_psf_overlap_nod
