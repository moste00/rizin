// SPDX-FileCopyrightText: 2022 Florian Märkl <info@florianmaerkl.de>
// SPDX-FileCopyrightText: 2022 GustavoLCR <gugulcr@gmail.com>
// SPDX-License-Identifier: LGPL-3.0-only

#include <rz_regex.h>
#include "minunit.h"
#include <rz_util/rz_str.h>
#include <rz_vector.h>

bool exec_regex(RzRegex *regex, const char *str, RzRegexMatch **out) {
	RzPVector *matches = rz_regex_match_all_not_grouped(regex, str, RZ_REGEX_ZERO_TERMINATED, 0, RZ_REGEX_DEFAULT);
	if (!matches || rz_pvector_empty(matches)) {
		return false;
	}
	*out = (RzRegexMatch *)rz_pvector_at(matches, 0);
	return true;
}

bool test_rz_regex_all_match(void) {
	RzRegex *reg = rz_regex_new("push", RZ_REGEX_EXTENDED, 0);
	mu_assert_notnull(reg, "Regex was NULL");
	RzRegexMatch *match = NULL;
	mu_assert_true(exec_regex(reg, "push", &match), "Regex match failed");
	mu_assert_notnull(match, "match was not set");
	mu_assert_eq(match->start, 0, "Start of match is not 0");
	mu_assert_eq(match->len, 4, "Len of match is not 4");
	rz_regex_free(reg);
	mu_end;
}

bool test_rz_regex_extend_space(void) {
	RzRegex *reg = rz_regex_new("push esi", RZ_REGEX_DEFAULT, 0);
	mu_assert_notnull(reg, "Regex was NULL");
	RzRegexMatch *match = NULL;
	mu_assert_notnull(reg, "Regex was NULL");
	mu_assert_true(exec_regex(reg, "push esi", &match), "Regex match failed. Was ' ' replaced with \\s in the pattern?");
	mu_assert_notnull(match, "match was not set");
	mu_assert_eq(match->start, 0, "Start of match is not 0");
	mu_assert_eq(match->len, 8, "Len of match is not 8");
	rz_regex_free(reg);
	mu_end;
}

bool test_rz_reg_exec(void) {
	const char *p = "abc|123";
	RzRegex *reg = rz_regex_new(p, RZ_REGEX_EXTENDED, 0);
	mu_assert_notnull(reg, "Regex was NULL");
	RzRegexMatch *match = NULL;
	mu_assert_true(exec_regex(reg, "abc", &match), "Regex match failed");
	mu_assert_notnull(match, "match was not set");
	mu_assert_eq(match->start, 0, "Start of match is not 0");
	mu_assert_eq(match->len, 3, "Len of match is not 3");
	mu_assert_true(exec_regex(reg, "zabc", &match), "Regex match failed");
	mu_assert_notnull(match, "match was not set");
	mu_assert_eq(match->start, 1, "Start of match is not 1");
	mu_assert_eq(match->len, 3, "Len of match is not 3");
	mu_assert_true(exec_regex(reg, "abcz", &match), "Regex match failed");
	mu_assert_notnull(match, "match was not set");
	mu_assert_eq(match->start, 0, "Start of match is not 0");
	mu_assert_eq(match->len, 3, "Len of match is not 3");
	mu_assert_true(exec_regex(reg, "123", &match), "Regex match failed");
	mu_assert_notnull(match, "match was not set");
	mu_assert_eq(match->start, 0, "Start of match is not 0");
	mu_assert_eq(match->len, 3, "Len of match is not 3");
	mu_assert_true(exec_regex(reg, "z123", &match), "Regex match failed");
	mu_assert_notnull(match, "match was not set");
	mu_assert_eq(match->start, 1, "Start of match is not 1");
	mu_assert_eq(match->len, 3, "Len of match is not 3");
	mu_assert_true(exec_regex(reg, "123z", &match), "Regex match failed");
	mu_assert_notnull(match, "match was not set");
	mu_assert_eq(match->start, 0, "Start of match is not 0");
	mu_assert_eq(match->len, 3, "Len of match is not 3");
	rz_regex_free(reg);
	const char *p_big = "\\d+(([abc]*d[efg])+|[123]4[567]+)*|[zyx]+(test)+[mnb]";
	reg = rz_regex_new(p_big, RZ_REGEX_EXTENDED, 0);
	mu_assert_true(exec_regex(reg, "z1abcde123z", &match), "Regex match failed");
	mu_assert_notnull(match, "match was not set");
	mu_assert_eq(match->start, 1, "Start of match is not 1");
	mu_assert_eq(match->len, 6, "Len of match is not 6");
	mu_assert_true(exec_regex(reg, "ayztesttestb123z", &match), "Regex match failed");
	mu_assert_notnull(match, "match was not set");
	mu_assert_eq(match->start, 1, "Start of match is not 1");
	mu_assert_eq(match->len, 11, "Len of match is not 11");
	rz_regex_free(reg);
	mu_end;
}

bool test_rz_regex_capture(void) {
	char *str = "abcd PrefixHello42s xyz";

	RzRegex *re = rz_regex_new("[a-zA-Z]*(H[a-z]+)([0-9]*)s", RZ_REGEX_EXTENDED, 0);
	mu_assert_notnull(re, "regex_new");

	RzPVector *matches = rz_regex_match_all_not_grouped(re, str, RZ_REGEX_ZERO_TERMINATED, 0, RZ_REGEX_DEFAULT);
	mu_assert_true(matches && !rz_pvector_empty(matches), "Regex match failed");
	mu_assert_eq(rz_pvector_len(matches), 3, "Regex match count failed.");

	RzRegexMatch *match = rz_pvector_at(matches, 0);
	mu_assert_eq(match->start, 5, "full match start");
	mu_assert_eq(match->len, 14, "full match len");
	char *s = rz_str_ndup(str + match->start, match->len);
	mu_assert_streq_free(s, "PrefixHello42s", "full match extract");

	match = rz_pvector_at(matches, 1);
	mu_assert_eq(match->start, 11, "capture 1 start");
	mu_assert_eq(match->len, 5, "capture 1 len");
	s = rz_str_ndup(str + match->start, match->len);
	mu_assert_streq_free(s, "Hello", "capture 1 extract");

	match = rz_pvector_at(matches, 2);
	mu_assert_eq(match->start, 16, "capture 2 start");
	mu_assert_eq(match->len, 2, "capture 2 len");
	s = rz_str_ndup(str + match->start, match->len);
	mu_assert_streq_free(s, "42", "capture 2 extract");

	rz_regex_free(re);
	mu_end;
}

int main() {
	mu_run_test(test_rz_regex_all_match);
	mu_run_test(test_rz_regex_extend_space);
	mu_run_test(test_rz_reg_exec);
	mu_run_test(test_rz_regex_capture);
}
