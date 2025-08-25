import pandas as pd
import numpy as np

# Constante pour le numéro de scénario
SCENARIO_NUM = 4


def read_csv_file(filepath):
    """Lit le fichier CSV et le traite"""
    try:
        df = pd.read_csv(filepath, sep=';')
        return df
    except Exception as e:
        print(f"Erreur lors de la lecture du fichier: {e}")
        return None


def format_value(value):
    """Formate une valeur avec une décimale, en évitant -0.0"""
    if pd.isna(value):
        return "0.0"
    rounded_val = round(float(value), 1)
    if rounded_val == -0.0:
        rounded_val = 0.0
    return f"{rounded_val:.1f}".replace('.', ',')


def create_latex_table(df, scenario_num=SCENARIO_NUM):
    """Crée le tableau LaTeX à partir du DataFrame avec des couleurs pour les valeurs"""
    estimator_mapping = {
        'Mono': ['monomode_expansion', 'HT_mono'],
        'Multi': ['multimode_expansion', 'HT_multi'],
        '1.a': ['1a'],
        "1.a'": ['1aprime'],
        '1.b': ['1b'],
        '2.a': ['2a'],
        "2.a'": ['2aprime'],
        '3.a': ['3a'],
        "3.a'": ['3aprime'],
        '3.b': ['3b'],
        "3.b'": ['3bprime'],
        '4': ['4']
    }

    unique_estimators = df['estimateur'].unique()
    has_3_single = '3' in unique_estimators
    has_3prime_single = '3prime' in unique_estimators
    estimator_mapping_sans_nr = estimator_mapping.copy()
    if has_3_single:
        estimator_mapping_sans_nr['3'] = ['3']
    if has_3prime_single:
        estimator_mapping_sans_nr["3'"] = ["3prime"]

    method_mapping = {
        'sans_nr': 'Sans non-réponse',
        'cnr_exacte': 'Avec probas de réponse exactes',
        'sans_grh': 'Avec probas de réponse estimées',
        'avec_grh': 'Avec probas estimées puis GRH'
    }

    year_mapping = {
        'y_1_': 'Y1',
        'y_2_': 'Y2',
        'y_3_': 'Y3'
    }

    estimator_order_default = ['Multi', 'Mono', '1.a', "1.a'",
                               '1.b', '2.a', "2.a'", '3.a', "3.a'", '3.b', "3.b'", '4']
    estimator_order_sans_nr = ['Multi', 'Mono',
                               '1.a', "1.a'", '1.b', '2.a', "2.a'"]
    if has_3_single:
        estimator_order_sans_nr.append('3')
    else:
        estimator_order_sans_nr.extend(['3.a', '3.b'])
    if has_3prime_single:
        estimator_order_sans_nr.append("3'")
    else:
        estimator_order_sans_nr.extend(["3.a'", "3.b'"])
    estimator_order_sans_nr.append('4')

    method_order = ['Sans non-réponse', 'Avec probas de réponse exactes',
                    'Avec probas de réponse estimées', 'Avec probas estimées puis GRH']
    year_order = ['Y1', 'Y2', 'Y3']

    latex_content = [
        r"\begin{table}[H]",
        r"\vspace{-0.4cm}",
        r"\definecolor{color1}{HTML}{5D6D7E}",
        r"\definecolor{color2}{HTML}{E74C3C}",
        r"\definecolor{color3}{HTML}{1E8449}",
        r"\begin{tabularx}{\textwidth}{@{}cXcc|cc|cc@{}}",
        r"\toprule",
        f"\\multicolumn{{2}}{{c}}{{\\textbf{{Scénario {scenario_num}}}}} & \\multicolumn{{2}}{{c}}{{$Y_1$ (exponentielle)}} & \\multicolumn{{2}}{{c}}{{$Y_2$ (parabolique)}} & \\multicolumn{{2}}{{c}}{{$Y_3$ (normale)}} \\\\",
        r"\cmidrule(lr){3-4}\cmidrule(lr){5-6}\cmidrule(l){7-8}",
        r"\multicolumn{2}{c}{Estimateur} & Biais & CV(EQM) & Biais & CV(EQM) & Biais & CV(EQM) \\",
        r"\midrule"
    ]

    for method in method_order:
        csv_method = None
        for k, v in method_mapping.items():
            if v == method:
                csv_method = k
                break
        if csv_method is None:
            continue
        method_data = df[df['methode'] == csv_method]
        if method == 'Sans non-réponse':
            current_estimator_mapping = estimator_mapping_sans_nr
            current_estimator_order = estimator_order_sans_nr
        else:
            current_estimator_mapping = estimator_mapping
            current_estimator_order = estimator_order_default
        num_estimators = len(current_estimator_order)
        first_row_prefix = f"\\multirow{{{num_estimators}}}{{*}}{{\\rotatebox{{90}}{{\\centering {method}}}}} & {current_estimator_order[0]} & "

        for i, estimator in enumerate(current_estimator_order):
            csv_estimator = None
            for candidate in current_estimator_mapping[estimator]:
                if candidate in method_data['estimateur'].values:
                    csv_estimator = candidate
                    break

            if method == "Avec probas estimées puis GRH" and estimator in ["3.a", "3.a'"]:
                if i == 0:
                    latex_content.append(
                        first_row_prefix + r"\multicolumn{6}{c}{\textit{Irréalisable}} \\")
                else:
                    latex_content.append(
                        f"& {estimator} & " + r"\multicolumn{6}{c}{\textit{Irréalisable}} \\")
                continue

            if csv_estimator is None:
                zero_cell = r"\textcolor{color1}{0.0}/\textcolor{color2}{0.0}/\textcolor{color3}{0.0}"
                if i == 0:
                    latex_content.append(
                        first_row_prefix + " & ".join([zero_cell]*6) + " \\\\")
                else:
                    latex_content.append(
                        f"& {estimator} & " + " & ".join([zero_cell]*6) + " \\\\")
                continue

            estimator_data = method_data[method_data['estimateur']
                                         == csv_estimator]
            row_values = []
            for year in year_order:
                csv_year = None
                for k, v in year_mapping.items():
                    if v == year:
                        csv_year = k
                        break
                if csv_year is None:
                    row_values.extend(
                        [r"\textcolor{color1}{0.0}/\textcolor{color2}{0.0}/\textcolor{color3}{0.0}"]*2)
                    continue
                year_data = estimator_data[estimator_data['y'] == csv_year]
                br_values = {"total": "0.0",
                             "strate_A": "0.0", "strate_B": "0.0"}
                for _, row in year_data.iterrows():
                    if row['ensemble'] in br_values:
                        br_values[row['ensemble']] = format_value(
                            row['biais_relatif_abs'])
                br_cell = f"\\textcolor{{color1}}{{{br_values['total']}}}/\\textcolor{{color2}}{{{br_values['strate_B']}}}/\\textcolor{{color3}}{{{br_values['strate_A']}}}"
                cv_values = {"total": "0.0",
                             "strate_A": "0.0", "strate_B": "0.0"}
                for _, row in year_data.iterrows():
                    if row['ensemble'] in cv_values:
                        cv_values[row['ensemble']] = format_value(
                            row['cv_reqm'])
                cv_cell = f"\\textcolor{{color1}}{{{cv_values['total']}}}/\\textcolor{{color2}}{{{cv_values['strate_B']}}}/\\textcolor{{color3}}{{{cv_values['strate_A']}}}"
                row_values.extend([br_cell, cv_cell])

            if i == 0:
                latex_content.append(first_row_prefix +
                                     " & ".join(row_values) + " \\\\")
            else:
                latex_content.append(
                    f"& {estimator} & " + " & ".join(row_values) + " \\\\")

        if method != method_order[-1]:
            latex_content.append(r"\midrule")

    latex_content.extend([
        r"\bottomrule",
        r"\end{tabularx}",
        f"""\\caption{{Valeur absolue des biais et coefficients de variation des EQM de chaque estimateur, selon le scénario {scenario_num} de mécanisme de réponse (en \\%) avec \\textcolor{{color1}}{{France entière}}, \\textcolor{{color2}}{{IdF}} et \\textcolor{{color3}}{{hors IdF}}}}""",
        r"\end{table}"
    ])
    return "\n".join(latex_content)


def main():
    csv_filename = "test_3.csv"
    df = read_csv_file(csv_filename)
    if df is not None:
        print("Aperçu des données:")
        print(df.head())
        print(f"\nColonnes disponibles: {list(df.columns)}")
        print(f"Méthodes uniques: {df['methode'].unique()}")
        print(f"Estimateurs uniques: {df['estimateur'].unique()}")
        print(f"Années uniques: {df['y'].unique()}")
        print(f"Ensembles uniques: {df['ensemble'].unique()}")
        latex_table = create_latex_table(df)
        print("\nTableau LaTeX généré :\n")
        print(latex_table)
    else:
        print("Impossible de lire le fichier CSV.")


if __name__ == "__main__":
    main()
