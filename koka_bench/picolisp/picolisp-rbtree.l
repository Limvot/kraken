#!/usr/bin/env bash
#{
# Thanks to http://rosettacode.org/wiki/Multiline_shebang#PicoLisp
exec pil $0 $1
# }#

(de cadddddr (l) (car (cddr (cdddr l))))

(de caddddr (l) (car (cdr (cdddr l))))

(de isRed (t) (let (   nA (car t)
                                            nB (cadr t)
                                            nC (caddr t)
                                            nD (cadddr t)
                                            nE (caddddr t)
                                            nF (cadddddr t)
                                        )
                                        (
                                            case nB
                                            ("R" T)
                                            (T NIL)
                                        )
                                    )
)

(de balanceLeft (l k v r) (let (   nA (car l)
                                    nB (cadr l)
                                    nC (caddr l)
                                    nD (cadddr l) 
                                    nE (caddddr l) 
                                    nF (cadddddr l)
                                    nLA (car nC)
                                    nLB (cadr nC)
                                    nLC (caddr nC)
                                    nLD (cadddr nC) 
                                    nLE (caddddr nC) 
                                    nLF (cadddddr nC)
                                    nRA (car nF)
                                    nRB (cadr nF)
                                    nRC (caddr nF)
                                    nRD (cadddr nF) 
                                    nRE (caddddr nF) 
                                    nRF (cadddddr nF)
                                )
                                (
                                    case nA
                                    ("N" (case (nLB)
                                               ("R" (list "N" "R" (list "N" "B" nLC nLD nLE nLF) nD nE (list "N" "B" nF k v r)))
                                               (T (case (nRB)
                                                   ("R" (list "N" "R" (list "N" "B" nC nD nE nRC) nRD nRE (list "N" "B" nRF k v r)))
                                                   (T (list "N" "B" (list "N" "R" nC nD nE nF) k v r))
                                               ))
                                         ))
                                    (T (list "L" "C" 0 0 0 0))
                                )
                            )


)

(de balanceRight (l k v r) (let (   nA (car r)
                                    nB (cadr r)
                                    nC (caddr r)
                                    nD (cadddr r) 
                                    nE (caddddr r) 
                                    nF (cadddddr r)
                                    nLA (car nC)
                                    nLB (cadr nC)
                                    nLC (caddr nC)
                                    nLD (cadddr nC) 
                                    nLE (caddddr nC) 
                                    nLF (cadddddr nC)
                                    nRA (car nF)
                                    nRB (cadr nF)
                                    nRC (caddr nF)
                                    nRD (cadddr nF)
                                    nRE (caddddr nF)
                                    nRF (cadddddr nF)
                                )
                                (
                                    case nA
                                    ("N" (case nLB
                                               ("R" (list "N" "R" (list "N" "B" l k v nLC) nLD nLE (list "N" "B" nLF nD nE nF)))
                                               (T (case nRB
                                                   ("R" (list "N" "R" (list "N" "B" l k v nC) nD nE (list "N" "B" nRC nRD nRE nRF)))
                                                   (T (list "N" "B" l k v (list "N" "R" nC nD nE nF)))
                                               ))
                                         ))
                                    (T (list "L" "C" 0 0 0 0))
                                )
                            )


)

(de ins (t k v)  (let (   nA (car t)
                                                nB (cadr t)
                                                nC (caddr t)
                                                nD (cadddr t)
                                                nE (caddddr t)
                                                nF (cadddddr t)
                                            )
                                            (
                                                case nB
                                                ("R" (cond ((< k nD) (list "N" "R" (ins nC k v) nD nE nF))
                                                        ((> k nD) (list "N" "R" nC nD nE (ins nF k v))) 
                                                        (T (list "N" "R" nC nD nE nF)) 
                                                    ))
                                                ("B" (cond ((< k nD) (if (isRed nC) (balanceLeft (ins nC k v) nD nE nF) (list "N" "B" (ins nC k v) nD nE nF)))
                                                        ((> k nD) (if (isRed nF) (balanceRight nC nD nE (ins nF k v)) (list "N" "B" nC nD nE (ins nF k v))))
                                                        (T (list "N" "B" nC nD nE nF)) 
                                                    ))
                                                (T (list "N" "R" (list "L" "C" 0 0 0 0) nD nE (list "L" "C" 0 0 0 0)))
                                            )
                                        )
)

(de setBlack (t)  (let (nA (car t)
                                             nB (cadr t)
                                             nC (caddr t)
                                             nD (cadddr t)
                                             nE (caddddr t)
                                             nF (cadddddr t)
                                            )
                                            (
                                                case nA
                                                ("N" (list nA "B" nC nD nE nF))
                                                (T t)
                                            )
                                       ) 
)

(de insertT (t k v)  (setBlack (ins t k v)))

(de foldT (t b)  (let (nA (car t)
                                             nB (cadr t)
                                             nC (caddr t)
                                             nD (cadddr t)
                                             nE (caddddr t)
                                             nF (cadddddr t)
                                            )
                                            (
                                                case nA
                                                ("N" (foldT nF (+ (foldT nC b) 1)))
                                                (T b)
                                            )
                                       )
)

(de makeTreeAux (n t) (cond ((<= n 0) t)
                            (T (let (nA (- n 1)
                                    ) (makeTreeAux nA (insertT t nA nA))
                               )
                            )
                      )
)

(de makeTree (n) (makeTreeAux n (list "L" "C" 0 0 0 0)))

(bye (println ( foldT (makeTree (car (str (opt)))) 0)))